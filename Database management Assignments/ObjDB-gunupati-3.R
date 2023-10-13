#Hierarchical Document Database
#Course:Database Management Systems
#Course Code:CS5200
#Author:Gowreesh Gunupati Venkata Sesha Sai
#Nuid:002647248


#importing library xfun to use file_ext,sans_ext functions#
library(xfun)


#created a global variable that is storing the value "docDB"
rootDir <- "docDB"

#Created a function configDB which is used to create a new file or directory##
configDB <- function(root,path){
  if(path==""&&(!file.exists(root))){
    dir.create(root)
  }
  else if (!file.exists(path)){
    dir.create(path)
  }
}

#created a function called strippedTag which is used for
#stripping the hashtag("#") from the tag when we passed
strippedTag <- function(tag){

  strippedTag1=substr(tag,2,nchar(tag))
  return (strippedTag1)
}

#created a function called genObjpath which is used for generating a path
#when we provide the the folder and a tag
genObjpath <- function(root,tag){
  
  tag=strippedTag(tag)
  result=paste(root,tag,sep="/")
  return (result)
}

#created a function called gettags which is used for generating a vector of
#hastags which are used in ihe srting 
getTags <- function(filename){
  
  filename=sans_ext(filename)
  tags=unlist(strsplit(filename,split=" #"))
  tags=tags[-1]
  for(i in 1:length(tags))
  {
    tags[i]=paste0("#",tags[i])
  }
  return (tags)
}

#created a fuction called getFilename where we return the trimmed name of the filename 
getFilename <- function(filename){
  name=sans_ext(filename)
  name=unlist(strsplit(name,split=" #"))
  extension=file_ext(filename)
  result=paste0(name[1],".",extension)
  n=substr(result,nchar(result),nchar(result))
  if(n==".")
   {
        result=substr(result,1,nchar(result)-1)
   }
  return (result)
}

# #Created a function called storeObjs which reads data from one file
# and create folders for the tags in the file names and then copy the renamed files
# in the respective tag folders
storeObjs <- function(folder, root, verbose){
  files <- list.files(path=folder)
  if(length(files)!=0)
  {
    for(i in 1:length(files))
    {
      n=getFilename(files[i])
      x=getTags(files[i])
      go=paste0("Copying ",n," to ")
    
      for(j in 1:length(x))
      {
        tag=strippedTag(x[j])
        objpath=genObjpath(root,x[j])
        configDB(root,objpath)
        y=paste0(folder,"/",files[i])
        file.copy(y,objpath)
        file.rename(paste0(objpath,"/",files[i]),paste0(objpath,"/",n))
        go=paste0(go,tag,",")
      }
      if(verbose==TRUE)
      {
      go=substr(go,1,(nchar(go)-1))
      print(paste0(go,"."))
      }
    }
  }  
  else
  {
    print("No files and directories exixts in the folder")
  }

}

#created a function clearDB() which deletes all files present
#in the root file but not the root
clearDB <- function(root){
  allfiles=paste0(root,"/*")
  unlink(allfiles,recursive = TRUE)
}

#created main funtion in which we call all the functions such as 
#getTags, configDb, genObjpath, storeObjs, getFilename and more 
main <- function(){
  
  ##################Assumptions Made##################
  
  #only the files which are in root directory are accesseble using the follwing code, if we need to 
  #access the files in other location other than the root directory please provide the path of folder in 
  #in folder attribute
  #In windows we could not create different files even if the filenames are case sensitive
  #we had assumed that for every filename the mainfilename,hashtag and hashtag,hashtag should have a space seperating them.
  
  ##################Test Cases########################
  
  #In the below code snippet we have called the function gettags, the function 
  #gives you the hastags present in the file name and the output will be in the form "#boston" "#northeastern"
  #"#enjoy"
  print(getTags("happy #boston #northeastern #enjoy.jpg"))
  
  #In the below code snippet we have called the function gettags, the function 
  #gives you the hastags present in the file name and the output will be in the form "#boston" "#northeastern"
  #"#enjoy"
  print(getTags("happy 0 #boston #northeastern #enjoy.jpg"))
  
  #In the below code snippet we have called the function gettags, the function 
  #gives you the hastags present in the file name and the output will be in the form "#boston" "#northeastern"
  #"#enjoy"
  print(getTags("happy.jpg #boston #northeastern #enjoy"))
  
  #In the below code snippet we have called the function gettags, the function 
  #gives you the hastags present in the file name and the output will be in the form "#boston" "#northeastern"
  #"#enjoy"
  print(getTags("happy #boston #northeastern #enjoy.png"))
  
  #In the below code snippet we have called the function getfilename, this function
  #extracts the first part of the filename without hashtags and output will be in the form "happy 0.jpg" 
  print(getFilename("happy 0 #boston #northeastern #enjoy.jpg"))
 
  #In the below code snippet we have called the function getfilename, this function
  #extracts the first part of the filename without hashtags and output will be in the form "happy.jpg"
  print(getFilename("happy #boston #northeastern #enjoy.jpg"))
  
  #In the below code snippet we have called the function getfilename, this function
  #extracts the first part of the filename without hashtags and output will be in the form "happy.jpg" 
  print(getFilename("happy.jpg #boston #northeastern #enjoy"))
 
  #In the below code snippet we have called the function getfilename, this function
  #extracts the first part of the filename without hashtags and output will be in the form "happy.png" 
  print(getFilename("happy.png #boston #northeastern #enjoy"))
  
  #In the below code snippet we have called the function getfilename, this function
  #extracts the first part of the filename without hashtags and output will be in the form "happy.png" 
  print(getFilename("happy #boston #northeastern #enjoy.png"))
  
  #In the below code snippet we have called the function genObjpath, this function creates the file path 
  #for the provided hashtags in the rootDir and output for the above snippet is "docDBboston" 
  print(genObjpath(rootDir,"#boston"))
 

  #calling configDB function to create a folder rootDir in working Directory
  configDB(rootDir,"")
  #calling StoreObjs function for copying files from firstpr1 to rootDir
  storeObjs("firstpr1",rootDir,TRUE)
  #calling ClearDB to clear all files in the folder ie rootDir
  clearDB(rootDir)
  
  
}
############calling main function############
main()
############Calling Quit function############
quit()
