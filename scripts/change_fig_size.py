from bs4 import BeautifulSoup

# Define the path to HTML file
html_file_path = 'index.html'

# New size for the images
new_height = 190
new_width = 220

# Read the HTML file
with open(html_file_path, 'r') as file:
    html_content = file.read()

# Parse the HTML
soup = BeautifulSoup(html_content, 'html.parser')

# Find all <img> tags and update their 'height' and 'width' attributes
for img_tag in soup.find_all('img'):
    img_tag['height'] = new_height
    img_tag['width'] = new_width

# Write the modified HTML back to the file (or a new file)
with open(html_file_path, 'w') as file:
    file.write(str(soup))

print("Updated image sizes in HTML.")
