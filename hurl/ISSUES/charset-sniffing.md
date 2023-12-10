# Optimize Charset Sniffing

Almost all charsets are supersets of ASCII, so when sniffing the charset for files which don't specify the encoding in their MIMEtype I can treat all the preceding text as ASCII. Though I suppose for this trick to work on UTF16 or UTF32 I'd need to remove any 0 bytes.
