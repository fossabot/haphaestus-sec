#include <fontconfig/fontconfig.h>
#include <stddef.h>

FcBool my_FcPatternAdd(FcPattern *p, const char *object,
        FcBool binding, FcBool append, FcValue *value) {
    if (binding) {
        return FcPatternAdd(p, object, *value, append);
    } else {
        return FcPatternAddWeak(p, object, *value, append);
    }
}

int size_value() {
    return sizeof(FcValue);
}
int size_matrix() {
    return sizeof(FcMatrix);
}
int size_PatternIter() {
    return sizeof(FcPatternIter);
}

int size_ConfigFileInfoIter() {
    return sizeof(FcConfigFileInfoIter);
}

int get_fontSet_nfont(FcFontSet *fonts) {
    return fonts->nfont;
}

FcPattern *get_fontSet_font(FcFontSet *fonts, int i) {
    if (i < 0) return NULL;
    if (i >= fonts->nfont) return NULL;
    if (i >= fonts->sfont) return NULL;
    if (fonts->fonts == NULL) return NULL;
    return fonts->fonts[i];
}
