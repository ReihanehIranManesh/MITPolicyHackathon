import os
import openai
import re
from googlesearch import search
import requests
from bs4 import BeautifulSoup

OPENAI_API_KEY = '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'

os.environ['OPENAI_API_KEY'] = OPENAI_API_KEY

from langchain.document_loaders import UnstructuredPDFLoader
from langchain.indexes import VectorstoreIndexCreator

text_folder = 'docs'
loaders = [UnstructuredPDFLoader(os.path.join(text_folder, fn)) for fn in os.listdir(text_folder)]

index = VectorstoreIndexCreator().from_loaders(loaders)

# question = "Please provide a bulleted list of all the troubleshooting issues discussed in the document."
#
# bullet_points = index.query(question)
#
# troubleshooting_list = bullet_points.strip().split('\n')
#
# troubleshooting_list = [item.strip('- ').strip() for item in troubleshooting_list]

question = "Please provide a bulleted list of all the tenant rights discussed in the document."

models = index.query(question)

models_list = models.strip().split('\n')

models_list = [item.strip('• ').strip() for item in models_list]


while (True):

    # print("Please choose an option from the following problems:")
    # num = 1
    # for problem in troubleshooting_list:
    #     print(str(num) + ". " + problem)
    #     num += 1
    #
    # problem = input("Enter the number: ")
    # problem = troubleshooting_list[int(problem) - 1]

    print("Please choose an option from the following models:")
    num = 1
    for model in models_list:
        print(str(num) + ". " + model)
        num += 1

    model = input("Enter the number: ")
    model = models_list[int(model) - 1]

    # Sample
    # search_service.run_chat("What is the color of an apple", "Quick")
    # question = "What may cause rough starting? Give me a step by step plan to fix it. Please be precise and provide an extensive answer. Can you give me the page number?"
    question = f"How can a tenant get help with {model}. Give me a step by step plan to help the tenant. Please be precise and provide an extensive answer. Can you give me the page number?"
    answer = index.query(question)
    print("Answer: ", answer)

    MODEL = "gpt-4-1106-preview"
    from openai import OpenAI

    client = OpenAI(
        api_key=OPENAI_API_KEY,
    )

    message = f"Based on the following chat history, give me one sentence that I can use to search the web to address the issue related to {model}. Question: {question} Answer: {answer}"

    stream = client.chat.completions.create(
        messages=[
            {
                "role": "user",
                "content": message,
            }
        ],
        model=MODEL,
        stream=True
    )
    web_search_keyword = ""
    for part in stream:
        web_search_keyword += part.choices[0].delta.content or ""

    # to search YouTube
    print("YouTube Links: ")
    query = "Give me YouTube links for" + web_search_keyword

    NUM = 3
    for j in search(query, tld="co.in", num=NUM, stop=10, pause=2):
        print(j)

    print("-----------------------------------------")

    # to search images
    print("Image Links: ")
    url = rf'https://www.google.no/search?q={web_search_keyword}&client=opera&hs=cTQ&source=lnms&tbm=isch&sa=X&safe=active&ved=0ahUKEwig3LOx4PzKAhWGFywKHZyZAAgQ_AUIBygB&biw=1920&bih=982'
    page = requests.get(url).text
    soup = BeautifulSoup(page, 'html.parser')

    results = []

    num = 0
    for raw_img in soup.find_all('img'):
        if num == NUM:
            break
        link = raw_img.get('src')
        if link and link.startswith("https://"):
            print(link)
            results.append(link)
            num += 1
            pass
        pass

    print("-----------------------------------------")

    # to search web

    print("Website Links: ")
    query = "Give me website links for" + web_search_keyword

    NUM = 3
    for j in search(query, tld="co.in", num=NUM, stop=10, pause=2):
        print(j)

    print("-----------------------------------------")
