from typing import List

from fastapi.responses import StreamingResponse
from llama_index.chat_engine.types import BaseChatEngine

from app.engine.index import get_chat_engine
from fastapi import APIRouter, Depends, HTTPException, Request, status
from llama_index.llms.base import ChatMessage
from llama_index.llms.types import MessageRole
from pydantic import BaseModel
import openai

import json 
import re
chat_router = r = APIRouter()


class _Message(BaseModel):
    role: MessageRole
    content: str


class _ChatData(BaseModel):
    messages: List[_Message]


def format_chat_messages(messages: List[ChatMessage]):

    history = """
    Here is the history of the conversation so far:
    """
    for m in messages:
        history += f"""
        {m.role.value}: {m.content}
        """
    history += "\n Now here is the most recent user message:\n"

    return history

def append_citations(response_obj):
    # Regular expression to find citations like [1], [2], etc.
    citation_pattern = r'\[\d+\]'
    
    # Extracting all the citations from the response text
    citations = re.findall(citation_pattern, response_obj.response)

    # Initialize the final response text with the original response text
    final_response = response_obj.response

    # Process each citation
    for citation in citations:
        # Extracting the citation number and adjusting for list indexing
        citation_number = int(citation.strip('[]')) - 1

        # Ensure the citation number is within the range of source_nodes
        if 0 <= citation_number < len(response_obj.source_nodes):
            # Retrieve the citation text
            citation_text = response_obj.source_nodes[citation_number].node.get_text()

            # Append the citation text to the final response
            final_response += f"<source_{citation_number+1}>" + citation_text + f"</source_{citation_number+1}>"

    return final_response



@r.post("")
async def chat(
    request: Request,
    data: _ChatData,
    chat_engine: any = Depends(get_chat_engine),
):
    # check preconditions and get last message
    if len(data.messages) == 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No messages provided",
        )
    lastMessage = data.messages.pop()
    if lastMessage.role != MessageRole.USER:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Last message must be from user",
        )
    # convert messages coming from the request to type ChatMessage
    messages = [
        ChatMessage(
            role=m.role,
            content=m.content,
        )
        for m in data.messages
    ]

    # query chat engine
    response = chat_engine.query(format_chat_messages(messages=messages) + lastMessage.content)

    

    print(response.response)
    response1 = append_citations(response)

    client = openai.OpenAI()

    response = client.chat.completions.create(
        model="gpt-4",
        messages=[
            {"role": "system", "content": "You are a helpful assistant whose job is to recommend follow-up questions for the user to ask based on the history of chat."},
            {"role": "user", "content": f'Here is the history of the conversation so far: {format_chat_messages(messages=messages) + lastMessage.content}. \n Now here is the most recent response from the system: {response.response}. \n Generate 3 follow-up questions for the user to ask that is relevant to the answer above. Output your answer in the following format: \n {{"question1": "question 1 example...", "question2": "question 2 example...", "question3": "question 3 example..."}}. \n Do not output anything else. Do not include new lines in your answer.'},
  ]
    )

    flups = response.choices[0].message.content

    return response1 + flups
    # stream response
    async def event_generator():
        for token in response:
            # If client closes connection, stop sending events
            if await request.is_disconnected():
                break
            yield token

    return StreamingResponse(event_generator(), media_type="text/plain")
