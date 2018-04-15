# rust-lci

*A LOLCODE interpreter written in Rust*

Target version: 1.2, following [the specs](https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md).

**Note: This is NOT a 100% faithful clone. Some minor things are changed if I think it makes more sense that way.**
Example:
```LOLCODE
SUM OF OBTW comment in the way TLDR 5 AN 4
```
In the original LOLCODE, that's a parsing error because the OBTW is read as an identifier.  
In my clone, this works perfectly fine (although I recommend not doing it, because it's hard to read).

There is no type casting in rust-lci either.  
This is because everything is already implicit where needed, so I don't see any point.

## Why?

I'm bored.  
I feel like porting more of my things to Web Assembly, but I ran out of things to port.  
I could've probably just ported the original LOLCODE directly, but meh.
