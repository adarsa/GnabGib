#################################################################################
#function require forecast and TTS package 
#dependent.ts is a timeseries with same number of elements as number of rows in ind_data
#dependent.ts is the dependent variable in regression and all the column in ind_data are 
#	taken as independent variables for building the regression model
#ind_data_forcst has columns with the same or less number of dependent variables for
#	the forecast horizon
##################################################################################
tslm_dynamic <- function(dependent.ts,ind_data,ind_data_forcst) {
	
	temp_data<-ind_data
	xnam<-colnames(temp_data)
	attach(temp_data)
	fmla<-as.formula(paste("dependent.ts~trend+season",paste(xnam,collapse="+")))
	regression<-tslm(fmla)# regression model for the data
	temp_data<-ind_data_forcst
	fcast<-forecast(regression,newdata=temp_data,h=nrow(ind_data_forcst))
	detach(temp_data)
	return(structure(fcast,class="forecast"))
}
