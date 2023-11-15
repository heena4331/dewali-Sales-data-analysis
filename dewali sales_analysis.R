

library(tidyverse)
library(ggplot2)
library(scales)

dewali_sales <- read_csv('C:/Users/Computer/Downloads/Python_Diwali_Sales_Analysis-main/Python_Diwali_Sales_Analysis-main/Diwali Sales Data.csv')

dewali_sales

glimpse(dewali_sales)
## data cleaning 

dewali_sales_df <- dewali_sales%>% select(-Status,-unnamed1)
dewali_sales_df <- na.omit(dewali_sales_df)

glimpse(dewali_sales_df)

dewali_sales_df$Amount <- as.integer(dewali_sales_df$Amount)


# EDA performance 

# gender wise count

ggplot(dewali_sales_df,aes(x= Gender ,fill= Gender,label =after_stat(count)))+
  
  geom_bar()+geom_text(stat = 'count',position = position_stack(vjust = 1.05),color= 'black')



# gender wise amount

# Plot the bar chart 
ggplot(sum_by_gender, aes(x = Gender, y =total_amount, fill = Gender)) +
  geom_bar(stat = 'identity') + scale_y_continuous(labels = scales:: label_comma(scale = 1e-3))
  geom_text(data = sum_by_gender, aes(label = total_amount), position = position_stack(vjust = 1.05), color = 'black') +
  labs(y = 'Amount') +
  theme_minimal()


  # Females have made the most purchases/orders."

# Age group and amount 

ggplot(dewali_sales_df,aes(x= `Age Group`,y= Amount , fill= Gender))+
  geom_bar(stat = 'identity')


# The 26-35 age group has placed the highest number of orders."


#top 10 5 states order wise
top_state <- dewali_sales_df%>% group_by(State)%>% summarise(count = n())%>%
  arrange(desc(count))%>% slice(1:5)

top_state

 ggplot(top_state,aes(x ="", y =count,fill= State))+
   geom_bar(width = 1,stat = 'identity',color='white')+
   coord_polar('y')+geom_text(aes(label= paste0(State)))

 
 
 # top 5 amount wise  state
 
 top_amunt_state <- dewali_sales_df%>% group_by(State)%>%summarise(amount = sum(Amount))%>%
   arrange(desc(amount))%>%slice(1:5)
 top_amunt_state 
 
 
 ggplot(top_amunt_state ,aes(x ="", y =amount,fill= State))+
   geom_bar(width = 1,stat = 'identity',color='white')+
   coord_polar('y')+geom_text(aes(label= paste0(State)))+
   scale_y_continuous(labels = scales::label_comma(scale = 1e-3))
 
 
 # from above graph we can see that most of the orders and spent amount both are from uttar pardesh,Maharashtra,
 # and Karnataka respectively
 
 
 
# marital_status by count orders
 
 
 dewali_sales_df$marital_status_label <- ifelse(dewali_sales_df$Marital_Status ==0 ,'married','Not married')

 glimpse( dewali_sales_df)
 
 
 
 ggplot(dewali_sales_df,aes(x = Marital_Status, fill =Gender))+
   geom_bar(position  = 'dodge')+ scale_x_continuous(breaks = c(0,1),labels = c('Married','Not married'))
 
 
 #Occupation wise amount 
 
 ggplot(dewali_sales_df,aes(x = Occupation, y = Amount , fill = Occupation))+
   geom_bar(stat = 'identity')+ scale_y_continuous(labels = scales:: label_comma(scale = 1e-3))
 
 #  It sector and healthcare occupation  have  spent highest amount
 
 
 # product category 
 # total order by category
 top_product_order <- dewali_sales_df%>% group_by(Product_Category)%>% summarise(total_order = sum( Orders))%>%
   arrange(desc(total_order))%>%slice(1:5)
 
 top_product_order
 
 ggplot(top_product_order,aes(x= Product_Category, y = total_order))+
   geom_bar(stat = 'identity',fill = 'skyblue')
 
 
 #total amount by category 
 top_product_amount <-  dewali_sales_df%>% group_by(Product_Category)%>% summarise(total_amount = sum(Amount))%>%
   arrange(desc(total_amount))%>%slice(1:5)
 
 top_product_amount
 
 
 ggplot(top_product_amount,aes(x= Product_Category, y = total_amount))+
   geom_bar(stat = 'identity',fill = 'skyblue')+scale_y_continuous(labels = scales:: label_comma(scale = 1e-3))

 #"In the above graph, we can see that the highest number of orders has been placed for clothing. However, in the second graph, 
 #we can observe that the highest amount of spending has not been in the clothing category but rather in the food category."
   
 
 
 
 
 #conclusion
 #The highest purchases have been made by married women in the age group of 26-35 years, residing in Uttar Pradesh, Maharashtra, and Karnataka.
 #They are employed in the IT sector and healthcare occupations, and they buy most products from the food, clothing, and electronics categories."
 
 

 