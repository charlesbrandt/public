# Version Control System

Keep your work in sync. 

Version control systems track changes to a set of files across different systems. When many people work on the same set of files, for example a software development team modifying code, a version control keeps those changes in sync. Version control is not limited to programming related tasks. If you use more than one computer and have a set of files that you edit on all of those computers, version control can help. 

Version control systems work best for text based files and smaller binary files.  For large binary files, you might be better off using a separate system (maybe rsync?) to synchronize changes. 


## Distributed vs. Centralized

In older systems, you would have to have access to a central repository in order to synchronize changes.  This was inconvenient if you were offline, or if the server was offline. 

Back in the early days of version control systems, the server was the single point of reference where any operation in the version control system could be performed. Checkouts and commits all required an active connection to the server. This made it difficult to work offline. 

These days modern version control systems are all distributed. This allows changes to be made without a direct connection to a centralized repository. Git is a good example. 

[ Git ](git.md)

distributed version control systems (DVCS)


