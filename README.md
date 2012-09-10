
The Cookie Jar
==============

This package handles incoming and outgoing cookies in the manner of a web browser.

The algorithms provided are described in section 5 of [RFC 6265, "HTTP State
Management Mechanism"]( http://www.rfc-editor.org/rfc/rfc6265.txt). Section 5
specifies client behaviors that maximize compatibility with common real-world
web servers.

Your questions, comments, and patches are welcome.


Future Improvements
-------------------

* Add another `Set-Cookie` header parser which accepts only the strict grammar
  defined in section 4 of RFC 6265. This is the grammar which web servers should
  use to ensure maximum compatibility with common real-world clients. Being able to
  validate against this grammar would be useful for testing servers.

* Add support for Unicode domain names, as specified by [RFC 5890,
  "Internationalized Domain Names for Applications"](
  http://www.rfc-editor.org/rfc/rfc5890.txt).

* Add support for configurable storage size limits and the corresponding eviction rules
  from section 5.3 of RFC 6265.

