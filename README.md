
# Configuration

create an .Rprofile file in the folder you plan to work with the package (eg./home/user/introtopics-project)

```{r, echo=FALSE}

paths <- list(data="/home/juho/drive/work/tutkimus/data/introtopics",
              root="/home/juho/projects/work/introtopics")

```



### Help with executing python from within r

(This is needed to compile the bugs models automatically)

Notice that rPython has to be installed MANUALLY by specifically setting the python version to use:

install.packages("rPython", configure.vars="RPYTHON_PYTHON_VERSION=3")

