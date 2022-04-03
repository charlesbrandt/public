# Virtual Environment

Python was a pioneer in isolating development dependencies so that they could be tracked and to ensure that the system space didn't get poluted. Now, I use containers as the swiss-army knife of isolated spaces. Works for other systems, not just python based ones. 

Install dependencies using pipenv:

    cd project_folder
    pipenv install moments

At this point you can launch the virtualenv shell (`pipenv shell`), or run individual commands within the environment (`pipenv run`). 


make a new virtualenv:

https://docs.python-guide.org/dev/virtualenvs/

