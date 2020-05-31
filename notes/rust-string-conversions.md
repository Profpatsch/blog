# Converting between different String types in Rust

```
let s: String = ...
let st: &str = ...
let u: &[u8] = ...
let b: [u8; 3] = b"foo"
let v: Vec<u8> = ...
let os: OsStrig = ...
let ost: OsStr = ...
 
From       To         Use                                    Comment
----       --         ---                                    -------
&str     -> String    String::from(st)
&str     -> &[u8]     st.as_bytes()
&str     -> Vec<u8>   st.as_bytes().to_owned()               via &[u8]

String   -> &str      &s                                     alt. s.as_str()
String   -> &[u8]     s.as_bytes()
String   -> Vec<u8>   s.into_bytes()
String   -> OsString  OsString::from(s)

&[u8]    -> &str      str::from_utf8(u).unwrap()
&[u8]    -> String    String::from_utf8(u).unwrap()
&[u8]    -> Vec<u8>   u.to_owned()
&[u8]    -> &OsStr    OsStr::from_bytes(u)                   use std::os::unix::ffi::OsStrExt;

[u8; 3]  -> &[u8]     &b[..]                                 byte literal
[u8; 3]  -> &[u8]     "foo".as_bytes()                       alternative via utf8 literal

Vec<u8>  -> &str      str::from_utf8(&v).unwrap()            via &[u8]
Vec<u8>  -> String    String::from_utf8(v)
Vec<u8>  -> &[u8]     &v

&OsStr   -> &str      ost.to_str().unwrap()
&OsStr   -> String    ost.to_owned().into_string().unwrap()  via OsString
&OsStr   -> Cow<str>  ost.to_string_lossy()                  Unicode replacement characters

OsString -> String    ost.into_string().unwrap()             returns original OsString on failure
```


## Source

Original source is [this document on Pastebin](https://web.archive.org/web/20190710121935/https://pastebin.com/Mhfc6b9i)
