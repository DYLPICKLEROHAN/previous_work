import pandas as pd, numpy as np, csv
import requests
import json
from datetime import datetime, timezone
import tkinter as tk
from tkinter import filedialog, messagebox

## Assessing phone number
def clean_phone(x):
    x = x.replace("'", "")
    x = x.replace(" ", "")
    x = x.replace("\+61", "0")
    x = x.replace("nan", "")
    
    if x.startswith('04') == False or len(x) < 10:
        x = np.nan
    else:
        x = x
    
    return x

## Assessing delivery type based on country
def int_or_dom_service(country):
    if country == 'AU':
        return 'PP'
    else:
        return 'STD'
    
# Obtaining cart info
def cart_contents(order_id, dataframe):
    order = dataframe[dataframe['orderNumber'] == order_id]
    cart = []
    for element in order['lineItems']:
        for item in range(0, len(element)):
            quantity = element[item].get('quantity')
            for n in range(0, quantity):
                cart.append(element[item].get('productName'))
    return cart

# Initialize a dictionary to store item classes
class Item:
    def __init__(self, name, length, width, height, weight, material, description, hs_tarrif, value):
        self.name = name
        self.length = length
        self.width = width
        self.height = height
        self.weight = weight
        self.material = material
        self.description = description
        self.hs_tariff = hs_tarrif
        self.value = value
        
    
inventory = []
# Replace 'your_file.csv' with the path to your CSV file
with open('inventory_properties.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)
    # Skip the header row if it exists
    next(csv_reader, None)

    for row in csv_reader:
        # Item	Length	Width	Height	Weight	Material	Description	Hs tarrif value
        item = Item(str(row[0]), str(row[1]), str(row[2]), str(row[3]), str(row[4]), 
                    str(row[5]), str(row[6]), str(row[7]), str(row[8]))
        inventory.append(item)
        
# Gathering package properties 
def package_properties(package_contents):
        length = 0
        width = 0
        height = 0
        weight = 0
        description = []
        
        for element in package_contents:
            for item in inventory:
                if item.name == element:                 # If the item in the order matches the name in the inventory list
                    height += float(item.height)           # Sum it's height
                    weight += float(item.weight)            # ... it's weight
                
                    if length < float(item.length):        # take the largest length
                        length = float(item.length)        
                    if width < float(item.width):          # and the widest width
                        width = float(item.width)           
                    description.append(str(item.description)) # and adds the item to the package description
        
        return length, width, height, weight, description

# Appending individual items
def defining_package(dataframe, inventory, dict_column):
    # Iterate through each row in dataframe
    for idx, row in dataframe.iterrows():
        # Extract the list of items for the current row including duplicates
        
        item_list = row[dict_column]
        
        package_index = 0
        
        # Iterate through each item in the list
        for item_name in item_list:
            # Skip 'Lottie and Jigglypuff'
            if item_name == 'Lottie and Jigglypuff':
                continue
            else:
             # Find item information in inventory list
                item_info = next((item for item in inventory if item.name == item_name), None)
                package_index += 1
                
                # If item information is found
                if item_info:
                # Increment package index
                    if package_index == 1:
                        dataframe.at[idx, "Parcel Contents - Description"] = item_info.description
                        dataframe.at[idx, "Parcel Contents - Weight"] = item_info.weight
                        dataframe.at[idx, "Parcel Contents - Value"] = item_info.value
                        dataframe.at[idx, "Parcel Contents - Quantity"] = "1"
                        dataframe.at[idx, "Parcel Contents - Country Of Origin"] = "AU"
                        dataframe.at[idx, "Parcel Contents - HS Tariff"] = item_info.hs_tariff
                # Fill package contents columns with item information
                    else:
                        dataframe.at[idx, f"Parcel {package_index} Contents - Description"] = item_info.description
                        dataframe.at[idx, f"Parcel {package_index} Contents - Weight"] = item_info.weight
                        dataframe.at[idx, f"Parcel {package_index} Contents - Value"] = item_info.value
                        dataframe.at[idx, f"Parcel {package_index} Contents - Quantity"] = '1'
                        dataframe.at[idx, f"Parcel {package_index} Contents - Country Of Origin"] = "AU"
                        dataframe.at[idx, f"Parcel {package_index} Contents - HS Tariff"] = item_info.hs_tariff
    return dataframe

def call_api():
    try:
        # Getting time
        current_time_utc = datetime.now(timezone.utc)
        formatted_time = current_time_utc.strftime('%Y-%m-%dT%H:%M:%SZ')

        # Using API to retrieve pending orders
        url = f"https://api.squarespace.com/1.0/commerce/orders?modifiedBefore={formatted_time}&modifiedAfter=2023-01-01T01:00:00Z&fulfillmentStatus={'PENDING'}"
        headers = {
            "Authorization": input('enter key'),
            "User-Agent": "me..",
            "Content-Type": "application/json"
        }

        response = requests.get(url, headers = headers)

        # Confirming 
        if response.status_code == 200:
            messagebox.showinfo("Success", "Successful connection to API, look for file named 'upload_this.csv'")
           
        elif response.status_code == 204:
            messagebox.showinfo('No content', 'There may not be any pending orders')
        else:
            messagebox.showwarning('Error', "Couldn't connect to API")
            
        # Compiling as dataframe
        pending_df = pd.json_normalize(response.json(), 'result')
        
        # Taking relevant information
        relevant_df = pending_df[['orderNumber','createdOn', 'customerEmail', 'shippingAddress.firstName', 
                                  'shippingAddress.lastName', 'shippingAddress.address1', 'shippingAddress.address2', 
                                  'shippingAddress.city', 'shippingAddress.state', 'shippingAddress.countryCode', 
                                  'shippingAddress.postalCode', 'shippingAddress.phone', 'lineItems','subtotal.value', 
                                  ]].copy()
        # Extending dataframe to explicitly write the shipping address

        # Shipping variables
        relevant_df.rename(columns={'shippingAddress.address1': 'Deliver To Address Line 1',
                                    'shippingAddress.address2': 'Deliver To Address Line 2',
                                    'shippingAddress.city': 'Deliver To Suburb',
                                    'shippingAddress.state': 'Deliver To State',
                                    'shippingAddress.postalCode': 'Deliver To Postcode',
                                    'shippingAddress.phone': 'Deliver To Phone Number',
                                    'shippingAddress.countryCode': 'Deliver To Country',
                                    'customerEmail' : 'Deliver To Email Address'
                                   }, inplace=True)
        relevant_df["Deliver To Address Line 3"] = np.nan
        relevant_df["Deliver To Phone Number"] = relevant_df['Deliver To Phone Number'].astype(str).apply(lambda x: clean_phone(x))
        relevant_df['Deliver To Name'] = relevant_df['shippingAddress.firstName'] + " " + relevant_df['shippingAddress.lastName']

        # Seller variables
        relevant_df["Send From Name"] = "Chloe Hayden ltd pty"
        relevant_df["Send From Business Name"] = "Chloe Hayden"
        relevant_df["Send From Address Line 1"] = "PO BOX 137"
        relevant_df["Send From Address Line 2"] = np.nan
        relevant_df["Send From Address Line 3"] = np.nan
        relevant_df["Send From Suburb"] = "Inverleigh"
        relevant_df["Send From State"] = "VIC"
        relevant_df["Send From Postcode"] = "3321"
        relevant_df["Send From Phone Number"] = np.nan
        relevant_df["Send From Email Address"] = "chloehaydensmerch@gmail.com"



        # Label variables
        relevant_df["Send Tracking Notifications"] = "NO"
        relevant_df["Send Tracking Email"] =  "NO"
        relevant_df["Additional Label Information 1"] = np.nan
        relevant_df["Delivery Instructions"] = np.nan
        relevant_df["Comments"] = "Thank You! :)"
        relevant_df["Importer's Reference Number"] = np.nan
        relevant_df["Licence Numbers"] = np.nan
        relevant_df["Certificate Numbers"] = np.nan
        relevant_df["Invoice Numbers"] = np.nan

        # Package variables
        relevant_df["Item Delivery Service"] = relevant_df["Deliver To Country"].apply(lambda x: int_or_dom_service(x))
        relevant_df["Item Packaging Type"] = 'OWN_PACKAGING'
        relevant_df["Item Dangerous Goods Flag"] = 'NO'

        # Set variables - other
        relevant_df["Signature On Delivery"] = 'NO'
        relevant_df["Extra Cover Amount"] = np.nan
        relevant_df["Cannot Be Delivered - Return To Sender / Abandon Parcel"] = 'RETURN'
        relevant_df["Customs Declaration - Commercial Value"] = "YES"
        relevant_df["Customs Declaration - Reason For Export"] = 'SALE_OF_GOODS'
        relevant_df["Customs Declaration - Why Are You Exporting These Goods"] = 'Delivery to Customers address'
        relevant_df["Export Declaration Number"] = np.nan
        relevant_df["SMS Tracking Notifications"] = 'NO'
        relevant_df["SMS Tracking Mobile Phone"] = np.nan
        relevant_df["Deliver To MyPost Number"] = np.nan
        relevant_df["Deliver To Business Name"] = np.nan
        relevant_df["Deliver To Type Of Address"] = np.nan
        # Creating column to hold new dictionary
        relevant_df["cart"] = relevant_df["orderNumber"].map(lambda x: cart_contents(x, relevant_df))

        # Create new columns in the orders_df DataFrame
        relevant_df['Item Length'] = relevant_df['cart'].apply(lambda x: package_properties(x)[0])
        relevant_df['Item Width'] = relevant_df['cart'].apply(lambda x: package_properties(x)[1])
        relevant_df['Item Height'] = relevant_df['cart'].apply(lambda x: package_properties(x)[2])
        relevant_df['Item Weight'] = relevant_df['cart'].apply(lambda x: package_properties(x)[3])
        relevant_df['Item Description'] = relevant_df['cart'].apply(lambda x: f"You are expecting {len(x)} item/s")
        
        final_df = defining_package(relevant_df, inventory, 'cart')
      
        # Keeping only columns relevant to Auspost
        columns = ['Send From Name', 'Send From Business Name', 'Send From Address Line 1',
                 'Send From Address Line 2','Send From Address Line 3','Send From Suburb',
                 'Send From State','Send From Postcode', 'Send From Phone Number', 'Send From Email Address',
                 'Deliver To Name',
                 'Deliver To Type Of Address', 'Deliver To Country', 'Deliver To Address Line 1',
                 'Deliver To Address Line 2', 'Deliver To Address Line 3','Deliver To Suburb',
                 'Deliver To State', 'Deliver To Postcode', 'Deliver To Phone Number',
                 'Deliver To Email Address', 'Item Packaging Type', 'Item Delivery Service', 
                 'Item Description', 'Item Length', 'Item Width',
                 'Item Height', 'Item Weight', 'Item Dangerous Goods Flag', 'Signature On Delivery',
                 'Extra Cover Amount', 'Cannot Be Delivered - Return To Sender / Abandon Parcel', 
                 'Customs Declaration - Commercial Value', 'Customs Declaration - Reason For Export', 
                 'Customs Declaration - Why Are You Exporting These Goods', 'Export Declaration Number', 
                 'SMS Tracking Notifications', 'SMS Tracking Mobile Phone', 'Send Tracking Notifications',
                 'Send Tracking Email', 'Additional Label Information 1', 'Delivery Instructions',
                 'Comments', "Importer's Reference Number", 'Licence Numbers',
                 'Certificate Numbers', 'Invoice Numbers']

        parcel_columns = [col for col in final_df.columns if col.startswith('Parcel')]
        all_cols = columns + parcel_columns 
        save = final_df[all_cols]
        save.to_csv('upload_this.csv')
        
    except Exception as e:
            messagebox.showerror("Error", f"An error occurred: {e}")
            

def main():
    root = tk.Tk()
    root.title("SQUARESPACE API CALL")

    btn_call_api = tk.Button(root, text="Get Orders", command=call_api)
    btn_call_api.pack(pady=20)

    root.mainloop()

if __name__ == "__main__":
    main()