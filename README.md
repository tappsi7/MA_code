# MA_code
Code to reproduce my MA thesis 


Ok so this is the code to reproduce the models and Graphs for my master project.
Enjoy this code which occupied me for the last year. 

Im sorry to anyone who has to read the code on how i got the count data. It is one of the most horendes lookings codes I  ever produced. 
But may the ugliness of this code be a testament to its human origins, since it is ugly in a way tha AI could never produce XD

The rest of the code should look fine though, by social science standards at least...... 

The counts_creation file was used to turn the DHS surveys into the cluster-cohort data using the R-Summer package. 
The data_creation file combines the count data wiht the covariates and creates the finished data and the neighborhood martix.
The model_estiamtion file contains the code the different Bayesian models were fit with as well as the glm model. 
The graph_creation file was used to create all tables and graphs in the PDF. It also contains the Leroux model and the Morans I test. 
An RDS file containing the finished output ot data_creation file is also included. This can be loaded into the model_estimation script to test the model.
The matrix is also here. Feel free to experiment! 
