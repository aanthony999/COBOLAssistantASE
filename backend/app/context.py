import os

from llama_index import ServiceContext
from llama_index.llms import OpenAI


def create_base_context():
    model = os.getenv("MODEL", "gpt-4")
    return ServiceContext.from_defaults(
        llm=OpenAI(model=model),
    )
