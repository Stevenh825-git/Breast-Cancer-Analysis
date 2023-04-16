# Query to clean the data set:
#   1. Removes the unnecessary columns
#   2. Fills in missing values for Patient_Status and Last_Visit_Date
#   3. Extracts Surgery Year
#   4. Orders data by the surgery date
SELECT  
  Age,Gender,Protein1,Protein2,Protein3,Protein4, Tumour_Stage, Histology, Surgery_type, Date_of_Surgery AS Surgery_Date,
  
  CASE # Fills Missing values in Last_Visit_Date column. 9999/01/01 means unknown
    WHEN Date_of_Last_Visit is NOT NULL THEN Date_of_Last_Visit 
    ELSE CAST("9999-01-01" AS DATE)
  END AS Last_Visit_Date,

  CASE # Fills Missing Values in Patient_Status Column
    WHEN Patient_Status is NULL THEN 'Unknown'
    ELSE Patient_Status
  END AS Patient_Status,

  EXTRACT(YEAR FROM Date_of_Surgery) AS Surgery_Year
FROM `breast-cancer-survival.Cancer_Data.Original_Data` 
ORDER BY Surgery_Date