import { Check, Copy } from "lucide-react";

import { Button } from "../button";
import ChatAvatar from "./chat-avatar";
import { Message } from "./chat.interface";
import Markdown from "./markdown";
import { useCopyToClipboard } from "./use-copy-to-clipboard";
import { useEffect, useState } from "react";

import ReactModal from 'react-modal';

export default function ChatMessage(chatMessage: Message) {
  const { isCopied, copyToClipboard } = useCopyToClipboard({ timeout: 2000 });

  function extractCitations(message: string) {
    // Regular expression to find citations like [1][2]
    const citationRegex = /\[\d+\]/g;

    // Find all matches for citation markers
    const markers = message.match(citationRegex);

    // Object to store citation details
    var citations = {};

    if (markers) {
      markers.forEach(marker => {
        // Extract the number from the marker
        const citationNumberMatch = marker.match(/\d+/);
        const citationNumber = citationNumberMatch ? citationNumberMatch[0] : '';

        // Regular expression to find the citation text within <source_x> tags
        const citationTextRegex = new RegExp(`<source_${citationNumber}>([\\s\\S]*?)<\/source_${citationNumber}>`);

        // Extract the citation text using the regex
        const citationTextMatch = message.match(citationTextRegex);
        const citationText = citationTextMatch ? citationTextMatch[1].trim() : "Citation text not found";

        // Store the citation text in the citations object
        citations = { ...citations, [citationNumber]: citationText };

        // Remove the detailed citation text from the message
        message = citationTextMatch ? message.replace(citationTextMatch[0], '') : message;
      });

      // Remove the citation markers from the message, except for the citation references
      message = message.replace(citationRegex, (match) => {
        const citationNumberMatch = match.match(/\d+/);
        const citationNumber = citationNumberMatch ? citationNumberMatch[0] : '';
        return `[${citationNumber}]`;
      });
    }

    // Extract and parse the JSON string at the end of the message
    console.log("message", message);
    var jsonString = message.substring(message.lastIndexOf("{"), message.length - 1);
    // remove all '/'s from the string to make it valid JSON
    jsonString = jsonString.replace(/\\/g, "");
    var flupQuestions = {};
    console.log(jsonString);
    try {
      flupQuestions = JSON.parse(jsonString);
      message = message.substring(0, message.lastIndexOf("{"));
    } catch (e) {
      console.log("errro", e)
      flupQuestions = {};
      // message = message.substring(0, message.lastIndexOf("{"));
    }



    // Return the updated message and the citations object
    return { updatedContent: message.replace(/"/g, ''), citations, flupQuestions };
  }

  const [updatedContent, setUpdatedContent] = useState<string>("");
  const [citations, setCitations] = useState<{}>({});
  const [popupVisible, setPopupVisible] = useState(false);
  const [selectedCitation, setSelectedCitation] = useState('');
  const [selectedCitationNumber, setSelectedCitationNumber] = useState('');

  useEffect(() => {
    const { updatedContent, citations, flupQuestions } = extractCitations(chatMessage.content);
    console.log(chatMessage.content);
    setUpdatedContent(updatedContent);
    setCitations(citations);
    chatMessage.setFlupQuestions(flupQuestions)
    console.log(flupQuestions);
  }, [chatMessage.content]);

  const handleCitationClick = (citationNumber: string) => {
    setSelectedCitation(citations[citationNumber]);
    setSelectedCitationNumber(citationNumber);
    setPopupVisible(true);
  };

  return (
    <>
    <div className="flex items-start gap-4 pr-5 pt-5">
      <ChatAvatar role={chatMessage.role} />
      <div className="group flex flex-1 justify-between gap-2">
        <div className="flex-1">
          <Markdown content={updatedContent} />
        </div>
        {Object.keys(citations).map((citationNumber) => (
            <Button
              key={citationNumber}
              onClick={() => handleCitationClick(citationNumber)}
              size="icon"
              variant="ghost"
              className="h-8 w-3"
            >
              [{citationNumber}]
            </Button>
          ))}
        <Button
          onClick={() => copyToClipboard(chatMessage.content)}
          size="icon"
          variant="ghost"
          className="h-8 w-8 opacity-0 group-hover:opacity-100"
        >
          {isCopied ? (
            <Check className="h-4 w-4" />
          ) : (
            <Copy className="h-4 w-4" />
          )}
        </Button>
        <ReactModal
        isOpen={popupVisible}
        onRequestClose={() => setPopupVisible(false)}
        contentLabel="Example Modal"
      >
        <h1>Source {selectedCitationNumber}</h1>
        
        <div>{selectedCitation}</div>
        <ReactModal
          isOpen={popupVisible}
          onRequestClose={() => setPopupVisible(false)}
          contentLabel="Example Modal"
        >
          <h1>Source {selectedCitationNumber}</h1>
          <div>{selectedCitation}</div>
          {/* BEGIN: ed8c6549bwf9 */}
          <button
            onClick={() => setPopupVisible(false)}
            className="bg-black text-white"
          >
            Close
          </button>
          {/* END: ed8c6549bwf9 */}
        </ReactModal>
      </ReactModal>
      </div>
    </div>
    </>
  );
}

