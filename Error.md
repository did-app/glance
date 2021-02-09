# Error
## Sorry Appologies

- There are a fine set of error kinds
- with the correct axes all error can be usefully represented.


A program runs functions, has inputs, uses services wether CLI, FAAS, Server, embedded
A type system might still leave invalid call values 1/0 being a classic example.

Go error as your domain

This is not about reducing to the smallest set of possible errors, but having a small set that is common

- Assertion Breakdown
  - Something assumed never to occur did.

- BadCall
  - 400 / Bad Request
  - A caller violated a condition that was known about from API
  - Badarg
- Declined Stopped UnProcessable
  - 422 
  - A caller violated a condition that it could not know from the API.
    - username unavailable
- NoLongerTrue Gone Expired 410
  - Special case of 422
  - A time has passed that now forbids this operation 
  - Even if the client/caller holds an expiry time they could not have access to a clock, latency might be high, clock skew
- Authentication Required
  - Special case of Bad Call Bad Invokation as API will define authentication value as required
- Forbidden unauthorized special case of 422

- Service unavailable
- Rate limited special case of service unavailable


Properties
- Retryable
- Knowable


Acl