# Support additional protocols
As mentioned in the README, I'd add like to add support for more URI schemes to
HURL. Here I'll list how I might implement them:

## ftp(s):
Network.FTP.Conduit.createSource looked convenient, but will likely be
incompatible with dependency versions.

Network.FTP.Client.retr, .withFTP(S), & possibly .login looks almost as easy.
Though I now that I tried it, I faced the same issues. Help updating this package
will be appreciated.

## magnet:
There's a `bittorrent` package, but it appears to be unmaintained and not up to
scratch for my needs.

## gopher:
Would need more conversion effort, to extract from the URL and convert them to
MIMEtypes. And if we want decent audio output, those would need to be parsed
according to sometimes undefined standards.

But should be easily implementable using the same APIs as for Gemini!
