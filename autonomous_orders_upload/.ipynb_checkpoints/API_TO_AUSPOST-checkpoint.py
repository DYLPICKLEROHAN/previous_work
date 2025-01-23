{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 42,
   "id": "8f7e91e1",
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Success\n",
      "Success\n",
      "Success\n"
     ]
    }
   ],
   "source": [
    "import pandas as pd, numpy as np, csv\n",
    "import requests\n",
    "import json\n",
    "from datetime import datetime, timezone\n",
    "import tkinter as tk\n",
    "from tkinter import filedialog, messagebox\n",
    "\n",
    "## Assessing phone number\n",
    "def clean_phone(x):\n",
    "    x = x.replace(\"'\", \"\")\n",
    "    x = x.replace(\" \", \"\")\n",
    "    x = x.replace(\"\\+61\", \"0\")\n",
    "    x = x.replace(\"nan\", \"\")\n",
    "    \n",
    "    if x.startswith('04') == False or len(x) < 10:\n",
    "        x = np.nan\n",
    "    else:\n",
    "        x = x\n",
    "    \n",
    "    return x\n",
    "\n",
    "## Assessing delivery type based on country\n",
    "def int_or_dom_service(country):\n",
    "    if country == 'AU':\n",
    "        return 'PP'\n",
    "    else:\n",
    "        return 'STD'\n",
    "    \n",
    "# Obtaining cart info\n",
    "def cart_contents(order_id, dataframe):\n",
    "    order = dataframe[dataframe['orderNumber'] == order_id]\n",
    "    cart = []\n",
    "    for element in order['lineItems']:\n",
    "        for item in range(0, len(element)):\n",
    "            quantity = element[item].get('quantity')\n",
    "            for n in range(0, quantity):\n",
    "                cart.append(element[item].get('productName'))\n",
    "    return cart\n",
    "\n",
    "# Initialize a dictionary to store item classes\n",
    "class Item:\n",
    "    def __init__(self, name, length, width, height, weight, material, description, hs_tarrif, value):\n",
    "        self.name = name\n",
    "        self.length = length\n",
    "        self.width = width\n",
    "        self.height = height\n",
    "        self.weight = weight\n",
    "        self.material = material\n",
    "        self.description = description\n",
    "        self.hs_tariff = hs_tarrif\n",
    "        self.value = value\n",
    "        \n",
    "    \n",
    "inventory = []\n",
    "# Replace 'your_file.csv' with the path to your CSV file\n",
    "with open('inventory_properties.csv', 'r') as csv_file:\n",
    "    csv_reader = csv.reader(csv_file)\n",
    "    # Skip the header row if it exists\n",
    "    next(csv_reader, None)\n",
    "\n",
    "    for row in csv_reader:\n",
    "        # Item\tLength\tWidth\tHeight\tWeight\tMaterial\tDescription\tHs tarrif value\n",
    "        item = Item(str(row[0]), str(row[1]), str(row[2]), str(row[3]), str(row[4]), \n",
    "                    str(row[5]), str(row[6]), str(row[7]), str(row[8]))\n",
    "        inventory.append(item)\n",
    "        \n",
    "# Gathering package properties \n",
    "def package_properties(package_contents):\n",
    "        length = 0\n",
    "        width = 0\n",
    "        height = 0\n",
    "        weight = 0\n",
    "        description = []\n",
    "        \n",
    "        for element in package_contents:\n",
    "            for item in inventory:\n",
    "                if item.name == element:                 # If the item in the order matches the name in the inventory list\n",
    "                    height += float(item.height)           # Sum it's height\n",
    "                    weight += float(item.weight)            # ... it's weight\n",
    "                \n",
    "                    if length < float(item.length):        # take the largest length\n",
    "                        length = float(item.length)        \n",
    "                    if width < float(item.width):          # and the widest width\n",
    "                        width = float(item.width)           \n",
    "                    description.append(str(item.description)) # and adds the item to the package description\n",
    "        \n",
    "        return length, width, height, weight, description\n",
    "\n",
    "# Appending individual items\n",
    "def defining_package(dataframe, inventory, dict_column):\n",
    "    # Iterate through each row in dataframe\n",
    "    for idx, row in dataframe.iterrows():\n",
    "        # Extract the list of items for the current row including duplicates\n",
    "        \n",
    "        item_list = row[dict_column]\n",
    "        \n",
    "        package_index = 0\n",
    "        \n",
    "        # Iterate through each item in the list\n",
    "        for item_name in item_list:\n",
    "            # Skip 'Lottie and Jigglypuff'\n",
    "            if item_name == 'Lottie and Jigglypuff':\n",
    "                continue\n",
    "            else:\n",
    "             # Find item information in inventory list\n",
    "                item_info = next((item for item in inventory if item.name == item_name), None)\n",
    "                package_index += 1\n",
    "                \n",
    "                # If item information is found\n",
    "                if item_info:\n",
    "                # Increment package index\n",
    "                    if package_index == 1:\n",
    "                        dataframe.at[idx, f\"Parcel Contents - Description\"] = item_info.description\n",
    "                        dataframe.at[idx, f\"Parcel Contents - Weight\"] = item_info.weight\n",
    "                        dataframe.at[idx, f\"Parcel Contents - Value\"] = item_info.value\n",
    "                        dataframe.at[idx, f\"Parcel Contents - Quantity\"] = \"1\"\n",
    "                        dataframe.at[idx, f\"Parcel Contents - Country Of Origin\"] = \"AU\"\n",
    "                        dataframe.at[idx, f\"Parcel Contents - HS Tariff\"] = item_info.hs_tariff\n",
    "                # Fill package contents columns with item information\n",
    "                    else:\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - Description\"] = item_info.description\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - Weight\"] = item_info.weight\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - Value\"] = item_info.value\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - Quantity\"] = '1'\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - Country Of Origin\"] = \"AU\"\n",
    "                        dataframe.at[idx, f\"Parcel {package_index} Contents - HS Tariff\"] = item_info.hs_tariff\n",
    "    return dataframe\n",
    "\n",
    "def call_api():\n",
    "    try:\n",
    "        # Getting time\n",
    "        current_time_utc = datetime.now(timezone.utc)\n",
    "        formatted_time = current_time_utc.strftime('%Y-%m-%dT%H:%M:%SZ')\n",
    "\n",
    "        # Using API to retrieve pending orders\n",
    "        url = f\"https://api.squarespace.com/1.0/commerce/orders?modifiedBefore={formatted_time}&modifiedAfter=2023-01-01T01:00:00Z&fulfillmentStatus={'PENDING'}\"\n",
    "        headers = {\n",
    "            \"Authorization\": \"Bearer 4d00ecf2-edd8-4cf4-a4e0-cff6c7917691\",\n",
    "            \"User-Agent\": \"me..\",\n",
    "            \"Content-Type\": \"application/json\"\n",
    "        }\n",
    "\n",
    "        response = requests.get(url, headers = headers)\n",
    "\n",
    "        # Confirming \n",
    "        if response.status_code == 200:\n",
    "            print('Success')\n",
    "        elif response.status_code == 204:\n",
    "            print('No content')\n",
    "        else:\n",
    "            print('Error')\n",
    "            \n",
    "        # Compiling as dataframe\n",
    "        pending_df = pd.json_normalize(response.json(), 'result')\n",
    "        \n",
    "        # Taking relevant information\n",
    "        relevant_df = pending_df[['orderNumber','createdOn', 'customerEmail', 'shippingAddress.firstName', \n",
    "                                  'shippingAddress.lastName', 'shippingAddress.address1', 'shippingAddress.address2', \n",
    "                                  'shippingAddress.city', 'shippingAddress.state', 'shippingAddress.countryCode', \n",
    "                                  'shippingAddress.postalCode', 'shippingAddress.phone', 'lineItems','subtotal.value', \n",
    "                                  ]].copy()\n",
    "        # Extending dataframe to explicitly write the shipping address\n",
    "\n",
    "        # Shipping variables\n",
    "        relevant_df.rename(columns={'shippingAddress.address1': 'Deliver To Address Line 1',\n",
    "                                    'shippingAddress.address2': 'Deliver To Address Line 2',\n",
    "                                    'shippingAddress.city': 'Deliver To Suburb',\n",
    "                                    'shippingAddress.state': 'Deliver To State',\n",
    "                                    'shippingAddress.postalCode': 'Deliver To Postcode',\n",
    "                                    'shippingAddress.phone': 'Deliver To Phone Number',\n",
    "                                    'shippingAddress.countryCode': 'Deliver To Country',\n",
    "                                    'customerEmail' : 'Deliver To Email Address'\n",
    "                                   }, inplace=True)\n",
    "        relevant_df[\"Deliver To Address Line 3\"] = np.nan\n",
    "        relevant_df[\"Deliver To Phone Number\"] = relevant_df['Deliver To Phone Number'].astype(str).apply(lambda x: clean_phone(x))\n",
    "        relevant_df['Deliver To Name'] = relevant_df['shippingAddress.firstName'] + \" \" + relevant_df['shippingAddress.lastName']\n",
    "\n",
    "        # Seller variables\n",
    "        relevant_df[\"Send From Name\"] = \"Chloe Hayden ltd pty\"\n",
    "        relevant_df[\"Send From Business Name\"] = \"Chloe Hayden\"\n",
    "        relevant_df[\"Send From Address Line 1\"] = \"PO BOX 137\"\n",
    "        relevant_df[\"Send From Address Line 2\"] = np.nan\n",
    "        relevant_df[\"Send From Address Line 3\"] = np.nan\n",
    "        relevant_df[\"Send From Suburb\"] = \"Inverleigh\"\n",
    "        relevant_df[\"Send From State\"] = \"VIC\"\n",
    "        relevant_df[\"Send From Postcode\"] = \"3321\"\n",
    "        relevant_df[\"Send From Phone Number\"] = np.nan\n",
    "        relevant_df[\"Send From Email Address\"] = \"chloehaydensmerch@gmail.com\"\n",
    "\n",
    "\n",
    "\n",
    "        # Label variables\n",
    "        relevant_df[\"Send Tracking Notifications\"] = \"NO\"\n",
    "        relevant_df[\"Send Tracking Email\"] =  \"NO\"\n",
    "        relevant_df[\"Additional Label Information 1\"] = np.nan\n",
    "        relevant_df[\"Delivery Instructions\"] = np.nan\n",
    "        relevant_df[\"Comments\"] = \"Thank You! :)\"\n",
    "        relevant_df[\"Importer's Reference Number\"] = np.nan\n",
    "        relevant_df[\"Licence Numbers\"] = np.nan\n",
    "        relevant_df[\"Certificate Numbers\"] = np.nan\n",
    "        relevant_df[\"Invoice Numbers\"] = np.nan\n",
    "\n",
    "        # Package variables\n",
    "        relevant_df[\"Item Delivery Service\"] = relevant_df[\"Deliver To Country\"].apply(lambda x: int_or_dom_service(x))\n",
    "        relevant_df[\"Item Packaging Type\"] = 'OWN_PACKAGING'\n",
    "        relevant_df[\"Item Dangerous Goods Flag\"] = 'NO'\n",
    "\n",
    "        # Set variables - other\n",
    "        relevant_df[\"Signature On Delivery\"] = 'NO'\n",
    "        relevant_df[\"Extra Cover Amount\"] = np.nan\n",
    "        relevant_df[\"Cannot Be Delivered - Return To Sender / Abandon Parcel\"] = 'RETURN'\n",
    "        relevant_df[\"Customs Declaration - Commercial Value\"] = \"YES\"\n",
    "        relevant_df[\"Customs Declaration - Reason For Export\"] = 'SALE_OF_GOODS'\n",
    "        relevant_df[\"Customs Declaration - Why Are You Exporting These Goods\"] = 'Delivery to Customers address'\n",
    "        relevant_df[\"Export Declaration Number\"] = np.nan\n",
    "        relevant_df[\"SMS Tracking Notifications\"] = 'NO'\n",
    "        relevant_df[\"SMS Tracking Mobile Phone\"] = np.nan\n",
    "        relevant_df[\"Deliver To MyPost Number\"] = np.nan\n",
    "        relevant_df[\"Deliver To Business Name\"] = np.nan\n",
    "        relevant_df[\"Deliver To Type Of Address\"] = np.nan\n",
    "        # Creating column to hold new dictionary\n",
    "        relevant_df[\"cart\"] = relevant_df[\"orderNumber\"].map(lambda x: cart_contents(x, relevant_df))\n",
    "\n",
    "        # Create new columns in the orders_df DataFrame\n",
    "        relevant_df['Item Length'] = relevant_df['cart'].apply(lambda x: package_properties(x)[0])\n",
    "        relevant_df['Item Width'] = relevant_df['cart'].apply(lambda x: package_properties(x)[1])\n",
    "        relevant_df['Item Height'] = relevant_df['cart'].apply(lambda x: package_properties(x)[2])\n",
    "        relevant_df['Item Weight'] = relevant_df['cart'].apply(lambda x: package_properties(x)[3])\n",
    "        relevant_df['Item Description'] = relevant_df['cart'].apply(lambda x: f\"You are expecting {len(x)} item/s\")\n",
    "        \n",
    "        final_df = defining_package(relevant_df, inventory, 'cart')\n",
    "      \n",
    "        # Keeping only columns relevant to Auspost\n",
    "        columns = ['Send From Name', 'Send From Business Name', 'Send From Address Line 1',\n",
    "                 'Send From Address Line 2','Send From Address Line 3','Send From Suburb',\n",
    "                 'Send From State','Send From Postcode', 'Send From Phone Number', 'Send From Email Address',\n",
    "                 'Deliver To Name',\n",
    "                 'Deliver To Type Of Address', 'Deliver To Country', 'Deliver To Address Line 1',\n",
    "                 'Deliver To Address Line 2', 'Deliver To Address Line 3','Deliver To Suburb',\n",
    "                 'Deliver To State', 'Deliver To Postcode', 'Deliver To Phone Number',\n",
    "                 'Deliver To Email Address', 'Item Packaging Type', 'Item Delivery Service', \n",
    "                 'Item Description', 'Item Length', 'Item Width',\n",
    "                 'Item Height', 'Item Weight', 'Item Dangerous Goods Flag', 'Signature On Delivery',\n",
    "                 'Extra Cover Amount', 'Cannot Be Delivered - Return To Sender / Abandon Parcel', \n",
    "                 'Customs Declaration - Commercial Value', 'Customs Declaration - Reason For Export', \n",
    "                 'Customs Declaration - Why Are You Exporting These Goods', 'Export Declaration Number', \n",
    "                 'SMS Tracking Notifications', 'SMS Tracking Mobile Phone', 'Send Tracking Notifications',\n",
    "                 'Send Tracking Email', 'Additional Label Information 1', 'Delivery Instructions',\n",
    "                 'Comments', \"Importer's Reference Number\", 'Licence Numbers',\n",
    "                 'Certificate Numbers', 'Invoice Numbers']\n",
    "\n",
    "        parcel_columns = [col for col in final_df.columns if col.startswith('Parcel')]\n",
    "        all_cols = columns + parcel_columns \n",
    "        save = final_df[all_cols]\n",
    "        save.to_csv('upload_this.csv')\n",
    "        \n",
    "    except Exception as e:\n",
    "            messagebox.showerror(\"Error\", f\"An error occurred: {e}\")\n",
    "            \n",
    "\n",
    "def main():\n",
    "    root = tk.Tk()\n",
    "    root.title(\"SQUARESPACE API CALL\")\n",
    "\n",
    "    btn_call_api = tk.Button(root, text=\"Get Orders\", command=call_api)\n",
    "    btn_call_api.pack(pady=20)\n",
    "\n",
    "    root.mainloop()\n",
    "\n",
    "if __name__ == \"__main__\":\n",
    "    main()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "485f0030",
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.10.9"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
