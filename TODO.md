
# TODO

## Add RFC references

Justify the code with reference to relevant RFCs.

## Sounder approach to serialization?

The only data type we serialize is SMTP Commands, and
it's handled by just one function, toByteString.
So I think it's not worth the trouble of creating instances
for e.g. the `cereal` package. 'AUTH' commands are serialized 
separately (and there is only one AUTH command implemented). 

## Improve error messages

If you try to send more commands after quitting: e.g. you send

```
>>> quit
>>> rset
```

then you'll get an error like this:

```
*** Exception: user error (Error executing smtp: UnexpectedResponse "not enough input")
```

Which means: the connection was closed, we couldn't
pull expected input. But a more user-friendly error message would
be nice.

Also, the `SMTPError` errors that are thrown are fairly terse.
It'd be nice to make those, also, more user-friendly.

