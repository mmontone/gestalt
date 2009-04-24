Some thoughts on using conditions restarts  as an interactive entry to our framework.

Suppose we have an interface to satisfy. We can:
1) Do nothing. For example, just document the thing.
2) Signal an error
3) Signal a warning.
4) Signal a restartable condition.

For example, in pwb we had permissions per button. The framework could ask you for adding the permission and generate the required codeas a consecuence.

Example:

There are not permissions defined for "view-list" in component MY-COMPONENT.
Restarts:
1) Its public
2) Set permission to the session's user
3) Choose a group
4) Implement manually

There could be a Web inteface too. We can generate the code fom that frontend because the code is in the server.


