#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <limits.h>
#include <ctype.h>
#include <sys/wait.h>

#include <dirent.h>
#include <errno.h>

#include "HsFFI.h"
#include <espeak-ng/espeak_ng.h>
#include <sndfile.h>

// #define WITH_SPEECHD // FIXME Doesn't support audio cues, navigation, or even read the full page.
#ifdef WITH_SPEECHD
#include <speechd_types.h>
#include <libspeechd.h>
#endif

/* Exported Haskell functions/types */
struct session;
struct session *c_newSession();
void c_freeSession(struct session*);

struct page;
struct page *c_initialReferer();
void *c_fetchURL(struct session*, char*, struct page*, char*);
//struct page **c_fetchURLs(struct session*, struct page*, char**); // FIXME segfaults.
void c_freePage(struct page*);

struct session *c_enableLogging(struct session*);
void c_writeLog(char*, struct session*);

char *c_renderDoc(struct session*, struct page*, _Bool);
char **c_extractLinks(struct page*, char *v2jProfile);
char **c_docLinksAndRendering(struct session*, struct page*, _Bool); // FIXME segfaults.

int c_ssmlHasMark(char*, char*);
char *c_formatLink(char *label, char *title, char *url);
char *c_dading();
char *c_dataDir(char *subdir);
char *c_recognizeIntent(char *profile); // For better JSON & subprocess APIs.

char *c_lastVisited(char*);

/* espeak-ng integration. Based on the espeak-ng command source code. */
SNDFILE *fd_wav = NULL;
char *path_wav = NULL;
static int samplerate;
espeak_ng_ERROR_CONTEXT context;

int choose_format(char *path) {
    SF_FORMAT_INFO	format_info;
    int k, count;

    sf_command(fd_wav, SFC_GET_FORMAT_MAJOR_COUNT, &count, sizeof (int));
    for (k = 0; k < count; k++) {
        format_info.format = k;
        sf_command(fd_wav, SFC_GET_FORMAT_MAJOR, &format_info, sizeof (format_info));
        char *suffix = path + strlen(path) - strlen(format_info.extension);
        if (strcmp(path, format_info.extension) == 0) return format_info.format;
    }
    return SF_FORMAT_WAV;
}

int paragraph_no = 0, section_no = 0;
int tablerow = 0, tablecol = 0, tableno = 0, in_table = 0;
int capture_marks(short *wav, int numsamples, espeak_EVENT *events) {
    while (events->type != 0) {
        if (events->type == espeakEVENT_MARK) {
            in_table = 0;
            if (sscanf(events->id.name, "-rhaps-paragraph%i", &paragraph_no) == 1) {}
            else if (sscanf(events->id.name, "-rhaps-section%i", &section_no) == 1) {}
            else if (sscanf(events->id.name, "-rhaps-tablecell%i:%ix%i",
                    &tableno, &tablerow, &tablecol) == 3) {in_table = 1;}
        }
        events++;
    }
    return 0;
}

int save_audio(short *wav, int numsamples, espeak_EVENT *events) {
    capture_marks(wav, numsamples, events);
    if (wav == NULL) return 0;

    while (events->type != 0) {
        if (events->type == espeakEVENT_SAMPLERATE) samplerate = events->id.number;
        events++;
    }

    if (fd_wav == NULL) {
        SF_INFO info;
        info.samplerate = samplerate;
        info.channels = 1;
        info.format = choose_format(path_wav) | SF_FORMAT_PCM_16 | SF_ENDIAN_LITTLE;
        fd_wav = sf_open(path_wav, SFM_WRITE, &info);
    }
    if (numsamples > 0) sf_writef_short(fd_wav, wav, numsamples);
    return 0;
}

int speak_initialize() {
    espeak_ng_InitializePath(NULL);
    context = NULL;
    espeak_ng_STATUS result = espeak_ng_Initialize(&context);
    if (result != ENS_OK) {
        espeak_ng_PrintStatusCodeMessage(result, stderr, context);
		espeak_ng_ClearErrorContext(&context);
		return 2;
    }

    if (path_wav != NULL) {
        result = espeak_ng_InitializeOutput(ENOUTPUT_MODE_SYNCHRONOUS, 0, NULL);
        espeak_SetSynthCallback(save_audio);
    } else {
        result = espeak_ng_InitializeOutput(ENOUTPUT_MODE_SPEAK_AUDIO, 0, NULL);
        espeak_SetSynthCallback(capture_marks);
    }
    if (result != ENS_OK) {
        espeak_ng_PrintStatusCodeMessage(result, stderr, context);
        return 3;
    }

    samplerate = espeak_ng_GetSampleRate();
    return 0;
}
void speak(char *ssml, char *mark, char* fallback) {
    int flags = espeakCHARS_AUTO | espeakPHONEMES | espeakENDPAUSE | espeakCHARS_UTF8 | espeakSSML;
    if (mark != NULL && c_ssmlHasMark(mark, ssml))
        espeak_Synth_Mark(ssml, strlen(ssml)+1, mark, 0, flags, NULL, NULL);
    else if (fallback != NULL && c_ssmlHasMark(fallback, ssml))
        espeak_Synth_Mark(ssml, strlen(ssml)+1, fallback, 0, flags, NULL, NULL);
    else espeak_Synth(ssml, strlen(ssml)+1, 0, POS_CHARACTER, 0, flags, NULL, NULL);
}

void speak_text(char *text, espeak_PARAMETER param, int value) {
    int curval = param != 0 ? espeak_GetParameter(param, 0) : 0;
    if (param != 0) espeak_SetParameter(param, curval + value, /* relative */0);

    int flags = espeakCHARS_AUTO | espeakPHONEMES | espeakENDPAUSE | espeakCHARS_UTF8;
    espeak_Synth(text, strlen(text)+1, 0, POS_CHARACTER, 0, flags, NULL, NULL);

    if (param != 0) espeak_SetParameter(param, curval, /* relative */0);
}

/* Utilities */
#define SWAP(type, a, b) {type temp = a; a = b; b = temp;}
int min(int a, int b) {return a < b ? a : b;}

/* Keyboard input */
int levenshtein_distance(const char *a, const char* b) {
    int a_len = strlen(a);
    int b_len = strlen(b);
    if (a_len < b_len) {
        SWAP(int, a_len, b_len);
        SWAP(const char*, a, b);
    }
    int *v0 = malloc(sizeof(int)*(b_len+1));
    int *v1 = malloc(sizeof(int)*(b_len+1));

    // Initialize v0 for an edit distance of an empty a
    for (int i = 0; i < b_len; i++) v0[i] = i;

    // The core algorithm
    for (int i = 0; i < a_len; i++) {
        // Edit distance from empty string is delete i characters.
        v1[0] = i;

        for (int j = 0; j < b_len; j++) {
            int deletion_cost = v0[j+1] + 1; // above cell + deletion
            int insertion_cost = v1[j] + 1; // left cell + insertion
            // top left cell + maybe substitution
            int substitution_cost = v0[j] + (tolower(a[i]) == tolower(b[j]) ? 0 : 1);

            v1[j+1] = min(deletion_cost, min(insertion_cost, substitution_cost));
        }

        // Progress to next row
        SWAP(int*, v0, v1);
    }

    int ret = v0[b_len];
    free(v0); free(v1);
    return ret;
}

int MAX_DIST = 3; // Is this ideal?
char *select_link(char **links, const char *command) {
    // Pass 1, min distance
    int score = INT_MAX;
    for (int i = 0; strcmp(links[i], " ") != 0; i++) {
        score = min(score, levenshtein_distance(command, links[i]));
    }

    // Pass 2: Is ambiguous?
    int num_matches = 0;
    for (int i = 0; strcmp(links[i], " ") != 0; i++)
        if (score >= levenshtein_distance(command, links[i]) - MAX_DIST) num_matches++;

    espeak_Cancel();

    // Pass 3: Retrieve answer
    for (int i = 0; strcmp(links[i], " ") != 0; i += 3) {
        if (command[0] != '\0' && command[0] != '\n') {// "" to read entire link table...
            if (score < levenshtein_distance(command, links[i]) &&
                score < levenshtein_distance(command, links[i+1]) &&
                score < levenshtein_distance(command, links[i+2])) continue;
            if (num_matches == 1) return links[i+2];
        }

        // Communicate
        printf("%s\t%s\t%s\n", links[i+2], links[i], links[i+1]);
        char *ssml = c_formatLink(links[i], links[i+1], links[i+2]);
        speak(ssml, NULL, NULL);
        free(ssml);
    }

    return NULL;
}

pid_t v2j_waitwake(char *v2j_profile) {
    pid_t pid = fork();
    if (pid == 0) {/* child */
        int pipefds[2];
        // Silence standard pipes, not a biggy if it fails.
        if (pipe(pipefds) == 0) dup2(pipefds[0], 0);
        if (pipe(pipefds) == 0) dup2(pipefds[1], 1);
        if (pipe(pipefds) == 0) dup2(pipefds[1], 2);
        execlp("voice2json", "voice2json", "--profile", v2j_profile, "wait-wake", "--exit-count", "1", NULL);
    }
    return pid;
}

volatile pid_t pid_waitwake = 0;
static void check_sigchld(int sig) {
    pid_t pid = 0;
    do {
        pid = waitpid(-1, NULL, WNOHANG);
        if (pid == pid_waitwake) pid_waitwake = 0;
    } while (pid > 0);
}

struct termios stored_settings, no_echo;
int read_keyboard = 1;
int use_wakeword = 1;
int speak_finalize(char *ssml, char *v2j_profile, char **links, char **out_link) {
    // Keyboard input mainloop.
    while (read_keyboard) {
        if (out_link != NULL && *out_link != NULL) return 0;

        char ch = getc(stdin);
        if (ch == '\033') {
            char mark[200];
            char fallback[200];
            espeak_Cancel();
            char c = getc(stdin);
            if (c == 0 || c == -1 || c == '\033' || c == 'q') goto close; // skip [
            switch (getc(stdin)) {
            case 'A':
                // 🠕
                if (in_table) {
                    tablerow--;
                    if (tablerow > 0) {
                        sprintf(mark, "-rhaps-tablecell%i:%ix%i", tableno, tablerow, tablecol);
                        speak(ssml, mark, NULL);
                        break;
                    } else in_table = 0;
                }
                section_no--;
                sprintf(mark, "-rhaps-section%i", section_no);
                speak(ssml, section_no > 0 ? mark : NULL, NULL);
                break;
            case 'B':
                // 🠗
                if (in_table) {
                    tablerow++;
                    sprintf(mark, "-rhaps-tablecell%i:%ix%i", tableno, tablerow, tablecol);
                    sprintf(fallback, "-rhaps-section%i", section_no+1);
                    speak(ssml, mark, fallback);
                    break; // FIXME What if that mark doesn't exist?
                }
                section_no++;
                sprintf(mark, "-rhaps-section%i", section_no);
                speak(ssml, section_no > 0 ? mark : NULL, NULL);
                break;
            case 'C':
                // ➔
                if (in_table) {
                    tablecol++;
                    sprintf(mark, "-rhaps-tablecell%i:%ix%i", tableno, tablerow, tablecol);
                    sprintf(fallback, "-rhaps-paragraph%i", paragraph_no+1);
                    speak(ssml, mark, fallback);
                    break; // FIXME What if that mark doesn't exist?
                }
                paragraph_no++;
                sprintf(mark, "-rhaps-paragraph%i", paragraph_no);
                speak(ssml, paragraph_no > 0 ? mark : NULL, NULL);
                break;
            case 'D':
                // 🠔
                if (in_table) {
                    tablecol--;
                    if (tablecol > 0) {
                        sprintf(mark, "-rhaps-tablecell%i:%ix%i", tableno, tablerow, tablecol);
                        speak(ssml, mark, NULL);
                        break;
                    } else in_table = 0;
                }
                paragraph_no--;
                sprintf(mark, "-rhaps-paragraph%i", paragraph_no);
                speak(ssml, paragraph_no > 0 ? mark : NULL, NULL);
                break;
            }
        } else if ((ch == ' ' && v2j_profile != NULL && *v2j_profile != '\0') ||
                (use_wakeword && pid_waitwake == 0)) {
            espeak_Cancel();
            speak(c_dading(), NULL, NULL);
            char *line = c_recognizeIntent(v2j_profile);
            if (out_link != NULL) {
                *out_link = select_link(links, line);
                if (*out_link == NULL) pid_waitwake = v2j_waitwake(v2j_profile);
            }
            free(line);
        } else if (ch > 0) {
            // Read in a line
            tcsetattr(0, TCSANOW, &stored_settings);
            char buffer[512];

            buffer[0] = ch;
            putchar(ch);

            char *line = buffer + 1;
            size_t len = 510;
            if (getline(&line, &len, stdin) < 0)
                perror("Failed to read stdin line");
            else if (out_link != NULL)
                *out_link = select_link(links, buffer);
            tcsetattr(0, TCSANOW, &no_echo);
        }
    }

    // Wakeword mainloop
    while (use_wakeword && out_link != NULL) {
        int status;
        pid_t child = v2j_waitwake(v2j_profile);
        if (child < 0) continue; // error
        if (waitpid(child, &status, 0) == 0) continue;
        espeak_Cancel();
        char *line = c_recognizeIntent(v2j_profile);
        *out_link = select_link(links, line);
        if (*out_link != NULL) return 0;
    }

    // Otherwise, wait until eSpeak has had it's say.
    espeak_ng_STATUS result = espeak_ng_Synchronize();
    if (result != ENS_OK) {
        espeak_ng_PrintStatusCodeMessage(result, stderr, context);
        return 4;
    }

close:
    if (path_wav != NULL) sf_close(fd_wav);
    espeak_ng_Terminate();
    return 0;
}

/* Main driver */
void write_links(FILE *fd_links, char **links) {
    for (int i = 0; strcmp(links[i], " ") != 0; i++) {
        fprintf(fd_links, "%s%c", links[i], (i % 3) == 2 ? '\n' : '\t');
    }
}

FILE *parse_opt_file() {
    FILE *ret = optarg != NULL ? fopen(optarg, "w") : stdout;
    if (ret == NULL) {
      fprintf(stderr, "Failed to open file %s\n", optarg);
      hs_exit();
      exit(-1);
    }
    return ret;
}

int dir_exists(char *path) {
    DIR *dir = opendir(path);
    if (dir) closedir(dir);
    else return ENOENT == errno;
    return 0;
}

int main(int argc, char **argv) {
    hs_init(&argc, &argv);
    int speak_err = 0;
    struct session *session = NULL;
    struct page *referer = NULL;

    int use_espeak = 0;
    int validate_v2j_profile = 0;
    char *logpath = NULL;
    char *mimes;
    FILE *fd_ssml = NULL;
    FILE *fd_links = NULL;
    char *v2j_profile = c_dataDir("voice2json");

    tcgetattr(0, &stored_settings);
    #ifdef WITH_SPEECHD
    SPDConnection *spd_conn = NULL;
    #endif
    mimes = "text/html text/xml application/xml application/xhtml+xml text/plain";

    int c;
    opterr = 0;
    #ifdef WITH_SPEECHD
    while ((c = getopt(argc, argv, "xs::l::L:kKv::VWw::dh")) != -1) {
    #else
    while ((c = getopt(argc, argv, "xs::l::kKv::VWw::h")) != -1) {
    #endif
        switch (c) {
        case 'x':
            mimes = "text/xml application/xml application/xhtml+xml text/html text/plain";
            break;
        case 's':
            fd_ssml = parse_opt_file();
            break;
        case 'l':
            fd_links = parse_opt_file();
            break;
        case 'L':
            logpath = optarg;
            break;
        case 'K':
            read_keyboard = 0;
            break;
        case 'k':
            read_keyboard = 1;
            break;
        case 'V':
            v2j_profile = "";
            validate_v2j_profile = 0;
            break;
        case 'v':
            if (optarg != NULL) v2j_profile = optarg;
            validate_v2j_profile = 1;
            break;
        case 'W':
            use_wakeword = 0;
            break;
        case 'w':
            use_espeak = 1;
            path_wav = optarg;
            break;
        #ifdef WITH_SPEECHD
        case 'd':
            spd_conn = spd_open("rhapsode", "main", NULL, SPD_MODE_SINGLE);
            if (spd_conn == NULL) fprintf(stderr, "Failed to open SpeechD connection, ignoring\n");
            spd_set_data_mode(spd_conn, SPD_DATA_SSML);
            break;
        #endif
        case '?':
            fprintf(stderr, "Invalid flag %c\n\n", optopt);
        case 'h':
            fprintf(stderr, "USAGE: rhapsode [FLAGS] URL...\n");
            fprintf(stderr, "\t-x\tX(HT)ML\tIndicates to expect an X(HT)ML file.\n");
            fprintf(stderr, "\t-s\tsilent/SSML\tWrites SSML to the specified file or stdout.\n");
            fprintf(stderr, "\t\t\thttps://xkcd.com/1692/\n");
            fprintf(stderr, "\t-l\tlinks\tWrite extracted links to specifed file or stdout as TSV.\n");
            fprintf(stderr, "\t-L\tlog\tWrite (append) network request timing to specified filepath.\n");
            fprintf(stderr, "\t-k\tkeyboard in\tRead arrow key navigation & links from stdin. Default behaviour, noop.\n");
            fprintf(stderr, "\t-K\t\tDon't read input from stdin.");
            fprintf(stderr, "\t-v\tvoice in\tEnsure voice input is enabled & optionally sets the Voice2JSON profile.\n");
            fprintf(stderr, "\t-V\t\tDon't listen for voice input.\n");
            fprintf(stderr, "\t-W\t\tNo wakeword\tDon't listen for the configured/trained wakeword regardless if voice recognition is enabled.\n");
            fprintf(stderr, "\t-w\t.wav\tWrite an audio recording of the webpage, or (DEFAULT) immediately output through speakers.\n");
            #ifdef WITH_SPEECHD
            fprintf(stderr, "\t-d\tSpeechD\tSchedule page read via the SpeechD daemon. (BROKEN)\n");
            #endif
            fprintf(stderr, "\t-h\thelp\tOutputs this usage information to stderr.\n");
            fprintf(stderr, "\t\t\tIf both -s & -l are enabled without an argument, writes to stderr instead.\n");

            hs_exit();
            return c == 'h' ? 0 : 1;
        }
    }
    if (read_keyboard) {
        // Read input character by character, not line by line.
        no_echo = stored_settings;
        no_echo.c_lflag &= (~ICANON);
        no_echo.c_lflag &= (~ECHO);
        no_echo.c_cc[VTIME] = 0;
        no_echo.c_cc[VMIN] = 1;
        tcsetattr(0, TCSANOW, &no_echo);
    }
    if (fd_ssml == stdout && fd_links == stdout) fd_links = stderr;
    #ifdef WITH_SPEECHD
    if (fd_ssml == NULL && fd_links == NULL && spd_conn == NULL && !use_espeak)
    #else
    if (fd_ssml == NULL && fd_links == NULL && !use_espeak)
    #endif
        use_espeak = 1;

#if 0 // Unit-test levenshtein_distance
    int dist = levenshtein_distance("kitten", "kitten");
    printf("kitten => kitten: %i\n", dist);
    if (dist != 0) return 10;
    dist = levenshtein_distance("kitten", "sitting");
    printf("kitten => sitting: %i\n", dist);
    if (dist != 3) return 11;
#endif

    session = c_newSession();
    if (logpath != NULL) session = c_enableLogging(session);
    referer = c_initialReferer();

    if (use_espeak) speak_err = speak_initialize();

    if (validate_v2j_profile && dir_exists(v2j_profile)) {
        // TODO Would packagers want a way to configure autodownloading of these profiles?
        fprintf(stderr, "Voice2JSON profile is uninitialized! Voice recognition is now disabled.\n");
        fprintf(stderr, "Please download a profile from: http://voice2json.org/#supported-languages\n");
        fprintf(stderr, "And extract the download to: %s\n", v2j_profile);
        if (use_espeak) {
            speak_text("Voice2JSON profile is uninitialized! Voice recognition is now disabled.",
                espeakRATE, 0);
        }

        v2j_profile = "";
    } else if (dir_exists(v2j_profile)) v2j_profile = "";
    use_wakeword &= *v2j_profile != '\0';

    char *ssml, **links, *uri;
    int read_links = 0;
    if (optind >= argc) {
        // No URLs specified, restore previous session or goto about:welcome
        uri = c_lastVisited("about:welcome");
        goto read_uri;
    }
    for (int i = optind; i < argc; i++) {
        uri = argv[i];

read_uri:
        if (use_espeak && speak_err == 0) speak_text(uri, espeakRATE, 10);
        #ifdef WITH_SPEECHD
        else if (spd_conn != NULL) spd_say(spd_conn, SPD_MESSAGE, uri);
        #endif
        else printf("%s\n", argv[i]);

        if (logpath != NULL) session = c_enableLogging(session);

        struct page *page = c_fetchURL(session, mimes, referer, uri);
        links = c_extractLinks(page, v2j_profile);
        ssml = c_renderDoc(session, page, use_espeak);

        if (logpath != NULL) c_writeLog(logpath, session);

        if (fd_ssml != NULL) fprintf(fd_ssml, "%s\n", ssml);
        if (read_keyboard && use_espeak && strcmp(uri, "about:welcome") == 0) read_links = 1;
        if (fd_links != NULL) write_links(fd_links, links);
        if (use_espeak & speak_err == 0) speak(ssml, "main", NULL);
        #ifdef WITH_SPEECHD
        if (spd_conn != NULL) spd_say(spd_conn, SPD_MESSAGE, ssml);
        #endif

        c_freePage(page);
    }

    uri = NULL;
    if (read_links && speak_err == 0) {
        speak_err = espeak_ng_Synchronize();
        if (speak_err == 0) select_link(links, "");
    }

    if (read_keyboard && use_wakeword && v2j_profile != NULL && *v2j_profile != 0) {
        // Interrupt read when `voice2json wait-wake` exits.
        pid_waitwake = v2j_waitwake(v2j_profile);
        if (pid_waitwake < 0) {
            fprintf(stderr, "Failed to run wakeword detection.\n");
            use_wakeword = 0;
        }
        else {
            struct sigaction act;
            memset(&act, 0, sizeof(act));
            act.sa_handler = check_sigchld;
            if (sigaction(SIGCHLD, &act, 0)) {
                fprintf(stderr, "Failed to wait upon wakeword detection.\n");
                use_wakeword = 0;
            }
        }
    }

    if (use_espeak & speak_err == 0) speak_err = speak_finalize(ssml, v2j_profile, links, &uri);
    if (uri != NULL) goto read_uri;

    #ifdef WITH_SPEECHD
    if (spd_conn != NULL) spd_close(spd_conn);
    #endif

    if (pid_waitwake > 0) {
        kill(pid_waitwake, SIGTERM);
        kill(pid_waitwake+1, SIGTERM); // Yuck, likely fragile! Also kill voice2json wait-wake's subprocesses.
    }

    c_freePage(referer);
    c_freeSession(session);
    hs_exit();
    tcsetattr(0, TCSANOW, &stored_settings);

    return speak_err;
}
