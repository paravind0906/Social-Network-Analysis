# Social-Network-Analysis

Amazon is one of the largest e-commerce platform and this project focuses on Amazon’s co-purchase network; i.e., which products are purchased together on Amazon. There are two data files for this project: “products.csv” and “copurchase.csv”. 
“products.csv” contains the information about the products. “copurchase.csv” contains the co-purchase information (i.e., if people bought one product, then they also co-purchased the other).

The file “products.csv” has the following variables: 
•	Id: Product id (number 1, 2, ..., 262109)
•	title: Name/title of the product
•	group: Product group (Book, DVD, Video or Music etc.), in this assignment, we only work with the books group.
•	salesrank: Amazon Salesrank (click the link to see the description of Salesrank)
•	rating: User rating on a scale of 1 to 5 
•	total number of reviews: Total number of product reviews available on Amazon

The file “copurchase.csv” has two variables, which are Product ID’s:
•	Source: This is the focal product that was purchased 
•	Target: People who bought “Source” also purchased the Target product 

