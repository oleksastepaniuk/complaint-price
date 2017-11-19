
# complaint_price
Code and data used for modeling the effect of changing the price of complaint to Anti-Monopoly Committee in the case of problems in public procurement


Currently, in Ukraine, there is a fixed price of complaint to Anti-Monopoly Committee in the case of problems in public procurement tender. As a result, in relative terms price of a complaint is very high for the participants of small tenders and very low for the participants of large tenders. Ministry of Economic Development of Ukraine intends to improve the situation by changing the fixed price of a complaint with a price of complaint set as a percentage of the reserve price of the auction.

Using the simulation model based on the expected profit from submitting a claim I show that complaints on general conditions of the tender and complaints on decisions regarding the winner of the auction have different expected benefit for the participant of the auction. As a result, Ministry should introduce separate prices for these two types of complaints.

If price of complaint on general conditions of the tender will be equal to 0.06% of the reserve price and price of complaint on decision regarding the winner of the auction will be equal to 0.3%, it will be profitable to submit a complaint if the profit of the participant is at least 2% of the reserve price and probability of a complaint being satisfied is at least 70%.

Model shows how new price will affect the total number of complaints and, as a result, revenues of the State Budget.

Files:
- KSE_AMCU_Complaints_price_19_11.ppt - presentation of the results (in Ukrainian)
  
- bi_downloads - folder with all the raw data used in the model + print screens that show what filters I used in the ProZorro BI to create each      file
  
- parameters -  folder with data (transformed from the raw data) and code used for calculating initial parameters of the model
  
- modeling - folder with data and code used for modeling the existing mechanism (with fixed price) and a new mechanism (price as % of the reserve price)
  
- result - folder with the results of executing code in parameters and modeling folders
