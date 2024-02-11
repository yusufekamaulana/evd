import scrapy
from selenium import webdriver
from selenium.webdriver.common.by import By

class LaptopSpider(scrapy.Spider):
    name = "laptop"
    allowed_domains = ["versus.com"]
    start_urls = [f"https://versus.com/id/laptop?filter[]=brands%3DAcer,Apple,Asus,Dell,HP,Huawei,Infinix,Lenovo,LG,Microsoft,MSI,Panasonic,Samsung,Realme,Sony,Toshiba,Xiaomi&page={i}&sort=release_date" for i in range(1, 32)]

    def parse(self, response):
        urls = response.css('div.List__item___7Ul-S a::attr(href)').getall()
        for url in urls:
            if "versus.com" not in url:
                url = response.urljoin(url)
                yield scrapy.Request(url, callback=self.find_details)

    def find_details(self, response):
        # Use 'webdriver.Chrome()' instead of 'webdriver.ChromiumEdge()'
        driver = webdriver.ChromiumEdge()
        
        # selenium for price
        driver.get(response.url)
        driver.implicitly_wait(3)
        price_element = driver.find_element(By.CSS_SELECTOR, '.PriceTable__priceTable___WdoPL .PriceTable__price___1MLQ4')
        price_text = price_element.text

        photo = response.css('.comparison .imageContainer .modernImage img::attr(src)').get()
        nama_produk = response.css('div.summaryName::text').get()
        spesifikasi = ['group_design', 'group_display', 'group_performance', 'group_benchmarks', 'group_connectivity', 'group_battery', 'group_features']
        my_dict = {'Nama Produk': nama_produk, 'harga': price_text, 'photo':photo}

        for spec_group in spesifikasi:
            spec_values = response.css(f'#{spec_group} div.Property__property___1PAON div.Property__valueContainer___14Alj ::text').getall()
            spec_values = [index for index in spec_values if index != nama_produk and '\"' != index and ')' != index and '(' != index]

            for i in range(0, len(spec_values), 2):
                key = spec_values[i]
                value = spec_values[i + 1]
                my_dict[key] = value

        driver.quit()

        yield my_dict
