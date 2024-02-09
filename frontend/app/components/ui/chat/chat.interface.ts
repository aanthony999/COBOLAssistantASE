export interface Message {
  id: string;
  content: string;
  role: string;
  setFlupQuestions: (flupQuestions: any)=> void;
}

export interface FlupQuestions {
  question1?: string;
  question2?: string;
  question3?: string;
}

export interface ChatHandler {
  messages: Message[];
  input: string;
  isLoading: boolean;
  handleSubmit: (
    e: React.FormEvent<HTMLFormElement>,
    ops?: {
      data?: any;
    },
  ) => void;
  handleInputChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  reload?: () => void;
  stop?: () => void;
  onFileUpload?: (file: File) => Promise<void>;
  onFileError?: (errMsg: string) => void;
  setTextInput: (text: string) => void;
}
