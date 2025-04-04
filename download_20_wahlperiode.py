import os
import time
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from urllib.parse import urljoin

# Bundestags-Seite mit den Open Data Links
BASE_URL = "https://www.bundestag.de/services/opendata"

# Verzeichnis für die heruntergeladenen XML-Dateien
SAVE_DIR = "bundestag_xml_20wp"
os.makedirs(SAVE_DIR, exist_ok=True)

# Selenium WebDriver Setup
CHROMEDRIVER_PATH = "/Users/philipp/Downloads/chromedriver-mac-arm64/chromedriver"  # <- Anpassen!
options = Options()
options.add_argument("--headless")  # Unsichtbarer Modus (optional)
service = Service(CHROMEDRIVER_PATH)
driver = webdriver.Chrome(service=service, options=options)
wait = WebDriverWait(driver, 10)  # Wartezeit für dynamische Inhalte

def get_all_xml_links():
    """Holt ALLE XML-Links, indem durch das Carousel geklickt wird."""
    print(f"🔎 Öffne die Seite {BASE_URL} mit Selenium...")
    driver.get(BASE_URL)
    time.sleep(5)  # Erste Wartezeit für das Laden der Seite

    all_links = set()
    last_count = 0  # Speichert die Anzahl der vorher gefundenen Links

    while True:
        soup = BeautifulSoup(driver.page_source, "html.parser")
        
        # Suche alle XML-Links auf der aktuellen Seite
        links = soup.find_all("a", href=True)
        xml_links = {urljoin(BASE_URL, link["href"]) for link in links if link["href"].endswith(".xml") and "/20" in link["href"]}
        
        # Neue Links hinzufügen
        new_links = xml_links - all_links
        all_links.update(xml_links)
        
        print(f"✅ {len(new_links)} neue Links gefunden. Gesamt: {len(all_links)}")

        # ❗ NEU: Wenn nach einem Klick keine neuen Links gefunden wurden, abbrechen
        if len(all_links) == last_count:
            print("🚀 Alle Seiten wurden durchgeklickt!")
            break
        
        last_count = len(all_links)  # Update der vorherigen Link-Anzahl

        # Suche die "Weiter"-Pfeiltaste mit der Klasse "slick-next"
        try:
            next_button = wait.until(EC.presence_of_element_located((By.CLASS_NAME, "slick-next")))
            driver.execute_script("arguments[0].scrollIntoView();", next_button)
            time.sleep(1)

            # ❗ NEU: Klick über JavaScript erzwingen
            driver.execute_script("arguments[0].click();", next_button)

            time.sleep(3)  # Wartezeit für das Laden der nächsten 10 Einträge
        except Exception as e:
            print(f"⚠️ Kein Weiter-Button mehr vorhanden: {e}")
            break

    return list(all_links)

def download_xml_files(xml_links):
    """Lädt die gefundenen XML-Dateien herunter und speichert sie im Verzeichnis."""
    for i, link in enumerate(xml_links, 1):
        filename = link.split("/")[-1]
        filepath = os.path.join(SAVE_DIR, filename)

        print(f"📥 Lade {filename} ({i}/{len(xml_links)}) herunter...")
        response = requests.get(link, stream=True)
        response.raise_for_status()

        with open(filepath, "wb") as file:
            for chunk in response.iter_content(chunk_size=8192):
                file.write(chunk)

        print(f"✅ Gespeichert: {filepath}")

if __name__ == "__main__":
    xml_links = get_all_xml_links()
    
    if xml_links:
        download_xml_files(xml_links)
        print("\n🚀 Download abgeschlossen!")
    else:
        print("⚠️ Keine XML-Dateien gefunden!")

    driver.quit()