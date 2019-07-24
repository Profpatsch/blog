# Converting between different String types in Rust

```
let s:String = ...
let st:&str = ...
let u:&[u8] = ...
let v:Vec<u8> = ...
 
From       To        Use                            Comment
----       --        ---                            -------
&str    -> String    String::from(st)
&str    -> &[u8]     st.as_bytes()
&str    -> Vec<u8>   st.as_bytes().to_owned()       via &[u8]

String  -> &str      &s                             alt. s.as_str()
String  -> &[u8]     s.as_bytes()
String  -> Vec<u8>   s.into_bytes()

&[u8]   -> &str      str::from_utf8(u).unwrap()
&[u8]   -> String    String::from_utf8(u).unwrap()
&[u8]   -> Vec<u8>   u.to_owned()

Vec<u8> -> &str      str::from_utf8(&v).unwrap()    via &[u8]
Vec<u8> -> String    String::from_utf8(v)
```


## Source

Original source is [this document on Pastebin](https://web.archive.org/web/20190710121935/https://pastebin.com/Mhfc6b9i)
