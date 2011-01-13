-module(http_helpers).

-export([error_to_code/1]).

error_to_code(Error) ->
    case Error of
						% 400 Bad Request
	bad_parameters -> 400;
	missing_parameters -> 400;
						% 401 Unauthorized
	unauthorized -> 401;
						% 403 Forbidden
	bad_credentials -> 403;
						% 404 Not Found
	not_found -> 404;
						% 408 Request Timeout
	request_timeout -> 408;
						% 409 Conflict
	conflict -> 409;
						% 500 Internal Server Error
	unexpected_error -> 500;
						% 501 Not Implemented
	not_implemented -> 501;
						% Default error
	_ -> 500
    end.
