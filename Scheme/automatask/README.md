Automatask
Eric Thivierge

A Scheme program to manage a tree structured set of tasks.
A task is defined by a prelude, a body and their corresponding success and failure continuations, which are also task objects.
The purpose of an automatask is to build a tree of tasks like lego blocks and to use the failure continuations for troubleshooting the cause of the failure.
