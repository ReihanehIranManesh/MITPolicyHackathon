import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

# file_path = 'eviction-lab.xlsx'
# eviction_data = pd.read_excel(file_path)
#
# grouped_data = eviction_data.groupby('name').agg({
#     'population': 'mean',  # Average population
#     'poverty-rate': 'mean',  # Average poverty rate
#     'pct-renter-occupied': 'mean',  # Average percentage of renter-occupied properties
#     'median-gross-rent': 'mean',  # Average median gross rent
#     'median-household-income': 'mean',  # Average median household income
#     'eviction-filings': 'mean',  # Average eviction filings
#     'pct-white': 'mean',  # Average percentage of white population
#     'pct-af-am': 'mean',  # Average percentage of African American population
#     # Other racial demographics can be included if needed
# }).reset_index()
#
# # Sorting data by eviction filings to see the areas with the highest numbers
# grouped_data_sorted = grouped_data.sort_values(by='eviction-filings', ascending=False)
#
# grouped_data_sorted.head()  # Displaying the top 5 areas with highest eviction filings
#
#
#
#  # Setting the aesthetic style of the plots
# sns.set_style("whitegrid")
#
# # Creating a bar chart for Income vs. Eviction Filings
# plt.figure(figsize=(12, 6))
# sns.barplot(x='median-household-income', y='eviction-filings', data=grouped_data_sorted.head(10))
# plt.title('Eviction Filings vs. Median Household Income in Top 10 Areas of Virginia')
# plt.xlabel('Median Household Income')
# plt.ylabel('Average Eviction Filings')
# plt.xticks(rotation=45)
# plt.show()


# Loading the dataset for Maryland
file_path_md = 'eviction-lab-md.xlsx'
eviction_data_md = pd.read_excel(file_path_md)

# Displaying the first few rows to verify the data and understand its structure
eviction_data_md.head()

# Identifying the unique counties in the Maryland dataset
md_counties = eviction_data_md['name'].unique()

# Creating charts for each county showing the trend of rent burden and eviction filings over the years

# Setting up the plotting area for separate y-axes for rent burden and eviction filings
plt.figure(figsize=(14, 7))

# Plotting Rent Burden for Maryland Counties
plt.subplot(1, 2, 1)
for county in md_counties:
    county_data = eviction_data_md[eviction_data_md['name'] == county]
    sns.lineplot(x='year', y='rent-burden', data=county_data, marker='o', label=county)
plt.title('Rent Burden Over Years by County in Maryland')
plt.xlabel('Year')
plt.ylabel('Rent Burden (%)')
plt.legend()

# Plotting Eviction Filings for Maryland Counties
plt.subplot(1, 2, 2)
for county in md_counties:
    county_data = eviction_data_md[eviction_data_md['name'] == county]
    sns.lineplot(x='year', y='eviction-filings', data=county_data, marker='o', label=county)
plt.title('Eviction Filings Over Years by County in Maryland')
plt.xlabel('Year')
plt.ylabel('Eviction Filings')
plt.legend()

plt.tight_layout()
plt.show()


