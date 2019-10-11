My Many Apps
Eric Thivierge

'My Many Apps' is a Java/JavaFX program that defines a framework to build 'App Stacks'.

==The App Stack==
An App Stack is a collection of individual Apps. The Apps are presented in tabular format (similar to a spreadsheet where each cell fills the view).
A stack has a dimension N and contains N * N Apps. The Apps use the services of the Core layer and can register themselves as a service for other Apps.

==App Messages==
The Stack Core provides services to the Apps.
* Inter-App and inter-Stack communication (e.g. Erlang message, Redis or RabbitMQ)
* Data persistence (e.g. MongoDB, Sqlite or filesystem)
* App and Core services registry

==Distributed stacks==
An App can be promoted to a full AppStack and be deployed to a remote node (e.g. Erlang process migration).
A tree of App Stacks can be built using this process. Nodes can be run in Docker containers on Kubernetes nodes.
Stacks and Apps can be public or private. An orphan Stack tree can be adopted by another Stack.

Because an App is only a single java thread viewed in a single panel, the cost of creating an App is minimized.

