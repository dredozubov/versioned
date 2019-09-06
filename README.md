# versioned
Protocol-agnostic data versioning framework

# Implementation goal

* versioned-core (this library)
* Protocol instance. We need schema-definitions. I propose JSON, so we need something like hjsonschema, but it's deprectated)
* versioned-swagger
* versioned-servant - turning schema mismatch into the particular HTTP error and responding with a useful response body(error message)
* versioned-servant-swagger - I'm not too familiar with servant-swagger, so I'm not sure how necessary this part is
