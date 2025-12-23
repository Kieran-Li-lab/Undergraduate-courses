import re

def setup():

    # 把json转换为python字典
    general_script = general_json
    script = doctor_json
    
    # 
    script = process_decomp_rules(script, general_script['tags'])
    
    # 内存输入和退出输入
    memory_inputs = general_script['memory_inputs']
    exit_inputs = general_script['exit_inputs']

    return general_script, script, memory_inputs, exit_inputs

def decompose(keyword, in_str, script):
    #初始化单词列表和查询到的标准答案
    comps = []
    answer_rule = ''

    # 遍历预定义的字典
    for d in script: 
        if d['keyword'] == keyword:
            # 遍历关键词的分解规则
            for rule in d['rules']:
                # 匹配分解规则
                m = re.match(rule['decomp'], in_str, re.IGNORECASE)
                if m:
                    # 按照分解规则分解输入
                    comps = list(m.groups())
                    answer_rule = rule['answer'][rule['last_used_answer_rule']]
                    #下次回复用的答案id+1
                    next_id = rule['last_used_answer_rule']+1
                    #如果下一个id超出了回复的种类，则回到0
                    if next_id >= len(rule['answer']):
                        next_id = 0
                    rule['last_used_answer_rule'] = next_id
                    break
            break
    return comps, answer_rule

def reassemble(components, answer_rule):
    #初始化回答
    response = 'Eliza: '
    #将查询得到的标准答案分解为多个单词
    answer_rule = answer_rule.split() 

    #遍历单词列表
    for comp in answer_rule:
        #如果是数字，则是在回答中添加之前分解的词语列表中该索引-1的词语
        if comp.isnumeric():
            response += components[int(comp)-1] + ' '
        #否则在回答中直接添加该单词
        else:
            response += comp + ' '

    # 删除尾部的空格
    response = response[:-1]

    return response

def process_decomp_rules(script, tags):
    # 遍历字典
    for d in script:
        # 遍历规则
        for rule in d['rules']:
            # 将分解规则转换为正则表达式
            rule['decomp'] = decomp_to_regex(rule['decomp'], tags) 
    return script

def preprocess_decomp_rule(in_str):
    # 形如(0 YOU 0)
    # 去掉括号
    in_str = re.sub('[()]', '', in_str)

    # 将字符串分隔为多个单词
    return in_str.split()


def decomp_to_regex(in_str, tags):
    out_str = ''

    in_str = preprocess_decomp_rule(in_str)

    for w in in_str:
        w = regexify(w, tags)
        # 用括号将句子正确划分为各个部分
        # \s* 匹配零个或多个空白字符 
        out_str += '(' + w + r')\s*' 

    return out_str

def regexify(w, tags):
    # 0 代表多个单词
    if w == '0': 
        w = '.*'
    # 此处匹配的就是w个单词，每个单词后允许有任意数量的空白字符
    elif w.isnumeric() and int(w) > 0:
        w = r'(?:\b\w+\b[\s\r\n]*){' + w + '}'
    # 单词的开始是@符号，代表标签
    elif w[0] == "@":
        # Get tag name
        tag_name = w[1:].lower()
        w = tag_to_regex(tag_name, tags)
    else:
        # 加上单词的界限\b
        w = r'\b' + w + r'\b'
    return w

def tag_to_regex(tag_name, tags):
    w = ''
    if tag_name in tags:
        # 将标签转换为正则表达式 (形如 x|y|z)
        w = r'\b(' + '|'.join(tags[tag_name]) + r')\b'
    return w

def generate_response(in_str, script, substitutions, memory_stack, memory_inputs):
    # 将输入分解为标点符号分隔的句子
    sentences = re.split(r'[.,!?](?!$)', in_str)

    # 获取输入中排名最高的单词的句子，并按排名对关键字进行排序
    sentence, sorted_keywords = retrieve(sentences, script, substitutions)
    # 查找匹配的分解规则
    for keyword in sorted_keywords:
        comps, answer_rule = decompose(keyword, sentence, script)
        if comps:
            response = reassemble(comps, answer_rule)
            #如果关键词为预定义的内存输入，将生成存入栈的回复答案
            if keyword in memory_inputs:
                    mem_comps, mem_answer_rule = decompose('^', sentence, script)
                    mem_response = reassemble(mem_comps, mem_answer_rule)
                    memory_stack.append(mem_response)
            break
    # 没有找到匹配的分解规则
    else:
        # 如果内存堆栈不为空，就从栈中pop出回答
        if memory_stack:
            response = memory_stack.pop()
        # 最后，实在匹配不到，给出通用答案
        else:
            comps, answer_rule = decompose('$', '$', script)
            response = reassemble(comps, answer_rule)
    #去掉多余空格
    response = ' '.join(response.split())
    #去掉标点符号
    response = re.sub(r'\s([?.!"](?:\s|$))', r'\1', response)
    #加上换行和前缀
    response += "\nYou: "
    return response

def retrieve(sentences, script, substitutions):

    # 遍历所有输入语句
    for i in range(0, len(sentences)):
        # 删除标点符号
        sentences[i] = re.sub(r'[^\w\s\']', '', sentences[i])
        # 替换关键词
        sentences[i] = replace(sentences[i], substitutions)

        if sentences[i]:
            #分解语句为词语
            keywords = sentences[i].lower().split()
            
            #初始化分数列表和判断标志
            ranks = []
            flag = False

            # 遍历关键词与预先记录的关键词进行匹配，查到其分数。
            for keyword in keywords:
                for d in script:
                    if d['keyword'] == keyword:
                        ranks.append(d['rank'])
                        flag = True
                        break
                # 如果没匹配到，记为0分。
                else:
                    ranks.append(0)
            if flag:
                #将关键词和排名两个列表合为一个元组的列表，每个元组中包含一对（关键词，排名）
                sorted_keywords = [x for _,x in sorted(zip(ranks, keywords), key=lambda pair: pair[0], reverse=True)]
                return  sentences[i],sorted_keywords
    return None, []

def replace(in_sentence, substitutions):
    out_sentence = ''

    # 遍历
    for word in in_sentence.split():
        # 如果与需替换的词匹配到，则替换为需要的词
        if word.lower() in substitutions:
            out_sentence += substitutions[word.lower()] + ' '
        # 否则还是加原来的词
        else:
            out_sentence += word + ' '

    return out_sentence

general_json = {
    "substitutions": {
        "you": "I",
        "i": "you",
        "am": "are",
        "are": "am",
        "your": "my",
        "my": "your",
        "were": "was",
        "me": "you",
        "you're": "i am",
        "i'm": "you are",
        "myself": "yourself",
        "yourself": "myself",
        "mom": "mother",
        "mum": "mother",
        "dad": "father",
        "dont": "don't",
        "cant": "can't",
        "cannot": "can't",
        "wont": "won't",
        "how": "what",
        "when": "what",
        "certainly": "yes",
        "yeah": "yes",
        "yea": "yes",
        "ye": "yes",
        "hello": "hi",
        "hey": "hi",
        "greetings": "hi",
        "maybe": "perhaps",
        "machine": "computer",
        "machines": "computer",
        "computers": "computer",
        "recollect": "remember",
        "recall": "remember",
        "forget": "forgot",
        "dreamed": "dreamt",
        "dreams": "dream",
        "everybody": "everyone",
        "nobody": "everyone",
        "noone": "everyone",
        "same": "like",
        "identical": "like",
        "equivalent": "like",
        "remind": "like",
        "alike": "like",
        "apologise": "sorry",
        "apologize": "sorry"
    },
    "tags": {
        "belief": [
            "belief",
            "feel",
            "think",
            "believe"
        ],
        "family": [
            "family",
            "mother",
            "father",
            "sister",
            "brother",
            "wife",
            "husband",
            "children",
            "child",
            "aunt",
            "uncle"
        ],
        "desire": [
            "desire",
            "want",
            "need"
        ],
        "everyone": [
            "everyone",
            "everybody",
            "nobody",
            "noone",
            "no one"
        ],
        "happy": [
            "happy",
            "elated",
            "glad",
            "better"
        ],
        "sad": [
            "sad",
            "unhappy",
            "depressed",
            "sick"
        ],
        "am": [
            "am",
            "is",
            "are",
            "was"
        ]
    },
    "memory_inputs": [
        "your"
    ],
    "exit_inputs": [
        "bye",
        "goodbye",
        "done",
        "quit",
        "exit",
        "bye.",
        "goodbye.",
        "done.",
        "quit.",
        "exit."
    ]
}

doctor_json = [
    {
        "keyword": "$",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Please go on.",
                    "I am not sure I understand you fully.",
                    "What does that suggest to you?",
                    "That is interesting. Please continue.",
                    "Tell me more about that.",
                    "Does talking about this bother you?"
                ],
                "last_used_answer_rule": 0
            }
        ],
        "_comment": "This is a placeholder keyword to be used when a generic answer is to be given."
    },
    {
        "keyword": "are",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 ARE YOU 0)",
                "answer": [
                    "Do you believe you are 4 ?",
                    "Would you want to be 4 ?",
                    "You wish I would tell you you are 4 ?",
                    "What would it mean if you were 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0)",
                "answer": [
                    "Why do you say 'am'?",
                    "I don't understand that."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "am",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 ARE I 0)",
                "answer": [
                    "Why are you interested in whether I am 4 or not?",
                    "Would you prefer if I weren't 4 ?",
                    "Do you sometimes think I am 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 ARE 0)",
                "answer": [
                    "Did you think they might not be 3 ?",
                    "Would you like it if they were not 3 ?",
                    "What if they were not 3 ?",
                    "Possibly they are 3 ."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "my",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 MY 0)",
                "answer": [
                    "Why are you concerned over my 3 ?",
                    "What about your own 3 ?",
                    "Are you worried about someone else's 3 ?",
                    "Really, my 3 ?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "was",
        "rank": 2,
        "rules": [
            {
                "decomp": "(0 WAS YOU 0)",
                "answer": [
                    "What if you were 4 ?",
                    "Do you think you were 4 ?",
                    "Were you 4 ?",
                    "What would it mean if you were 4 ?",
                    "What does 4 suggest to you?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU WAS 0)",
                "answer": [
                    "Were you really?",
                    "Why do you tell me you were 4 now?",
                    "Perhaps I already knew you were 4 ."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 WAS I 0)",
                "answer": [
                    "Would you like to believe I was 4 ?",
                    "What suggests that I was 4 ?",
                    "What do you think?",
                    "Perhaps I was 4 .",
                    "What if I had been 4 ?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "you",
        "rank": 1,
        "rules": [
            {
                "decomp": "(0 YOU @DESIRE 0)",
                "answer": [
                    "What would it mean to you if you got 5 ?",
                    "Why do you want 5 ?",
                    "Suppose you got 5 soon.",
                    "What if you never got 5 ?",
                    "What would getting 5 mean to you?",
                    "What does wanting 5 have to do with this discussion?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU ARE 0 @SAD 0)",
                "answer": [
                    "I am sorry to hear that you are 6 .",
                    "Do you think coming here will help you not to be 6 ?",
                    "I'm sure it's not pleasant to be 6 .",
                    "Can you explain what made you 6 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU ARE 0 @HAPPY 0)",
                "answer": [
                    "What makes you 6 just now?",
                    "Can you explain why you are 6 ?",
                    "How have I helped you to be 6 ?",
                    "Has your treatment made you 6 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU @BELIEF YOU 0)",
                "answer": [
                    "Do you really think so?",
                    "But you are not sure you 6 ?",
                    "Do you really doubt you 6 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU ARE 0)",
                "answer": [
                    "Is it because you are 4 that you came to me?",
                    "How long have you been 4 ?",
                    "Do you believe it is normal to be 4 ?",
                    "Do you enjoy being 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU CAN'T 0)",
                "answer": [
                    "How do you know you can't 4 ?",
                    "Have you tried?",
                    "Perhaps you could 4 now.",
                    "Do you really want to be able to 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU DON'T 0)",
                "answer": [
                    "Don't you really 4 ?",
                    "Why don't you 4 ?",
                    "Do you wish to be able to 4 ?",
                    "Does that trouble you?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU FEEL 0)",
                "answer": [
                    "Tell me more about such feelings.",
                    "Do you often feel 4 ?",
                    "Do you enjoy feeling 4 ?",
                    "What does feeling 4 remind you of?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOU 0 I 0)",
                "answer": [
                    "Do you 3 anyone else?",
                    "Do you wish to 3 me?",
                    "Perhaps in your fantasy we 3 each other.",
                    "You seem to need to 3 me."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0)",
                "answer": [
                    "You say 1 .",
                    "Can you elaborate on that?",
                    "Do you say 1 for some special reason?",
                    "That's quite interesting."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "i",
        "rank": 2,
        "rules": [
            {
                "decomp": "(0 I AM 0 BUT 0)",
                "answer": [
                    "What makes you think I am 4 ?",
                    "Does it please you to believe I am 4 ?",
                    "Do you sometimes wish you were 4 ?",
                    "Perhaps you would like to be 4 ."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 I AM 0)",
                "answer": [
                    "Does it please you to believe I am 4 ?",
                    "What makes you think I am 4 ?",
                    "Do you sometimes wish you were 4 ?",
                    "Perhaps you would like to be 4 ."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 I 0 YOU)",
                "answer": [
                    "Why do you think I 3 you?",
                    "You like to think I 3 you - don't you?",
                    "What makes you think I 3 you?",
                    "Really, I 3 you?",
                    "Do you wish to believe I 3 you?",
                    "Suppose I did 3 you - what would that mean?",
                    "Does someone else believe I 3 you?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 I 0)",
                "answer": [
                    "We were discussing you - not me.",
                    "Oh, I 3 ?",
                    "You're not really talking about me - are you?",
                    "What are your feelings now?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "your",
        "rank": 2,
        "rules": [
            {
                "decomp": "(0 YOUR @FAMILY 0 YOU)",
                "answer": [
                    "Who else in your family 5 you?",
                    "Your 4 5 you?",
                    "Your 4 ?",
                    "What else comes to mind when you think of your 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOUR 0 @FAMILY 0)",
                "answer": [
                    "Tell me more about your family.",
                    "Your 5 ?",
                    "What else comes to mind when you think of your 5 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOUR 0)",
                "answer": [
                    "Your 3 ?",
                    "Why do you say your 3 ?",
                    "Does that suggest anything else which belongs to you?",
                    "Is it important to you that 2 3 ?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "^",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 YOUR @FAMILY 0 YOU)",
                "answer": [
                    "Earlier you mentioned that your 4 5 you.",
                    "Would you like to discuss your 4 in more detail? Earlier you mentioned that your 4 5 you.",
                    "What else comes to mind when you think of your 4 ?",
                    "Does that have anything to do with the fact that your 4 5 you?",
                    "Tell me more about your family.",
                    "Let's discuss the fact that your 4 5 you further.",
                    "Apart from your 4 , who else is in your family?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOUR 0 @FAMILY 0)",
                "answer": [
                    "Does that have anything to do with the fact that your 5 6 ?",
                    "Let's discuss your 5 further.",
                    "Earlier you mentioned your 5 .",
                    "Tell me more about your family.",
                    "Apart from your 5 , who else is in your family?",
                    "Your 5 ?",
                    "What else comes to mind when you think of your 5 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 YOUR 0)",
                "answer": [
                    "Does that have anything to do with the fact that your 3 ?",
                    "Let's discuss your 3 further.",
                    "Earlier you mentioned your 3 .",
                    "Do you have any other thoughts on your 3 ?",
                    "What else do you think about your 3 ?"
                ],
                "last_used_answer_rule": 0
            }
        ],
        "_comment": "This is a placeholder keyword to be used when a response must be generated for the memory stack."
    },
    {
        "keyword": "can",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 CAN I 0)",
                "answer": [
                    "You believe I can 4 don't you?",
                    "You want me to be able to 4 ?",
                    "Perhaps you would like to 4 yourself."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 CAN YOU 0)",
                "answer": [
                    "Whether or not you can 4 depends on you more than on me.",
                    "Do you want to be able to 4 ?",
                    "Perhaps you don't want to 4 ."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "what",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Why do you ask?",
                    "Does that question interest you?",
                    "What is it you really want to know?",
                    "Are such questions much on your mind?",
                    "What answer would please you most?",
                    "What do you think?",
                    "What comes to your mind when you ask that?",
                    "Have you asked such question before?",
                    "Have you asked anyone else?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "because",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Is that the real reason?",
                    "Don't any other reasons come to mind?",
                    "Does that reason seem to explain anything else?",
                    "What other reasons might there be?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "why",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0 WHY DON'T I 0)",
                "answer": [
                    "Do you believe I don't 5 ?",
                    "Perhaps I will 5 in good time.",
                    "Should you 5 yourself?",
                    "You want me to 5 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 WHY CAN'T YOU 0)",
                "answer": [
                    "Do you think you should be able to 5 ?",
                    "Do you want to be able to 5 ?",
                    "Do you believe this will help you to 5 ?",
                    "Have you any idea why you can't 5 ?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "yes",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "You seem quite positive.",
                    "Are you sure?",
                    "I see.",
                    "I understand.",
                    "You are sure."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "no",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Why not?",
                    "Are you saying 'no' just to be  negative?",
                    "Why 'no'?",
                    "You are being a bit negative."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "sorry",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Please don't apologize.",
                    "Apologies are not necessary.",
                    "What feelings do you have when you apologize?",
                    "I've told you that apologies are not required."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "remember",
        "rank": 5,
        "rules": [
            {
                "decomp": "(0 YOU REMEMBER 0)",
                "answer": [
                    "Do you often think of 4 ?",
                    "Does thinking of 4 bring anything else to mind?",
                    "What else do you remember?",
                    "Why do you remember 4 just now?",
                    "What in the present situation reminds you of 4 ?",
                    "What is the connection between me and 4 ?",
                    "What else does 4 remind you of?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 DO I REMEMBER 0)",
                "answer": [
                    "Did you think I would forget 5 ?",
                    "Why do you think I should recall 5 now?",
                    "What about 5 ?",
                    "You mentioned 5 ."
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 I REMEMBER 0)",
                "answer": [
                    "How could I forget 4 ?",
                    "What about 4 should I remember?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "forgot",
        "rank": 5,
        "rules": [
            {
                "decomp": "(0 YOU FORGOT 0)",
                "answer": [
                    "Can you think of why you might forget 4 ?",
                    "Why can't you remember 4 ?",
                    "How often do you think of 4 ?",
                    "Does it bother you to forget that?",
                    "Could it be a mental block?",
                    "Are you generally forgetful?",
                    "Do you think you are suppressing 4 ?"
                ],
                "last_used_answer_rule": 0
            },
            {
                "decomp": "(0 DID I FORGOT 0)",
                "answer": [
                    "Why do you ask?",
                    "Are you sure you told me?",
                    "Would it bother you if I forgot 5 ?",
                    "Why should I recall 5 just now?",
                    "Tell me more about 5 ."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "everyone",
        "rank": 2,
        "rules": [
            {
                "decomp": "(0 @EVERYONE 0)",
                "answer": [
                    "Can you think of anyone in particular?",
                    "Really, 2 ?",
                    "Surely not 2 ?",
                    "Who, for example?",
                    "You are thinking of a very special person?",
                    "Who, may I ask?",
                    "Someone special, perhaps?",
                    "You have a particular person in mind, don't you?",
                    "Who do you think you're talking about?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "hi",
        "rank": 0,
        "rules": [
            {
                "decomp": "(HI 0)",
                "answer": [
                    "Hi.",
                    "Hello.",
                    "Welcome."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "always",
        "rank": 1,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Can you think of a specific example?",
                    "When?",
                    "What incident are you thinking of?",
                    "Really, always?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "like",
        "rank": 10,
        "rules": [
            {
                "decomp": "(0 @AM 0 LIKE 0)",
                "answer": [
                    "In what way?",
                    "What resemblance do you see?",
                    "What does that similarity suggest to you?",
                    "What other connections do you see?",
                    "What do you suppose that resemblance means?",
                    "What is the connection, do you suppose?",
                    "Could there really be some connection?",
                    "How?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "different",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "In what way?",
                    "How is it different?",
                    "What differences do you see?",
                    "What does that difference suggest to you?",
                    "What other distinctions do you see?",
                    "What do you suppose the disparity means?",
                    "How?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "if",
        "rank": 3,
        "rules": [
            {
                "decomp": "(0 IF 0)",
                "answer": [
                    "Do you think it's likely that 3 ?",
                    "Do you wish that 3 ?",
                    "What do you think about 3 ?",
                    "Really, 2 3 .",
                    "What would you do if 3 ?",
                    "But what are the chances that 3 ?",
                    "What does this speculation lead to?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "dreamt",
        "rank": 4,
        "rules": [
            {
                "decomp": "(0 I DREAMT 0)",
                "answer": [
                    "Really, 4 ?",
                    "Have you ever fantasized 4 while you were awake?",
                    "Have you ever dreamt 4 before?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "dream",
        "rank": 3,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "What does that dream suggest to you?",
                    "Do you dream often?",
                    "What persons appear in your dreams?",
                    "Do you believe that dreams have something to do with your problem?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "perhaps",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "You don't seem quite certain.",
                    "Why the uncertain tone?",
                    "Can't you be more positive?",
                    "You aren't sure?",
                    "Don't you know?",
                    "How likely, would you estimate?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "name",
        "rank": 15,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "I am not interested in names.",
                    "I've told you before, I don't care about names - please continue."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "deustch",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "I only speak English.",
                    "I told you before, I don't understand German."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "francais",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "I only speak English.",
                    "I told you before, I don't understand French."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "italiano",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "I only speak English.",
                    "I told you before, I don't understand Italian."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "espanol",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "I only speak English.",
                    "I told you before, I don't understand Spanish."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "hello",
        "rank": 0,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Hi. What seems to be your problem?",
                    "How do you do? Please state your problem."
                ],
                "last_used_answer_rule": 0
            }
        ]
    },
    {
        "keyword": "computer",
        "rank": 50,
        "rules": [
            {
                "decomp": "(0)",
                "answer": [
                    "Do computers worry you?",
                    "Why do you mention computers?",
                    "What do you think machines have to do with your problem?",
                    "Don't you think computers can help people?",
                    "What about machines worries you?",
                    "What do you think about machines?",
                    "You don't think I am a computer program, do you?"
                ],
                "last_used_answer_rule": 0
            }
        ]
    }
]


def main():
    memory_stack = []
    #从json文件中加载需要的字典
    general_script, script, memory_inputs, exit_inputs = setup()

    #获取用户的首次输入
    in_str = input("Eliza: Welcome.\nYou: ")
    in_str_l = in_str.lower()

    while in_str_l not in exit_inputs:

        if not in_str_l.islower():
            response = ('Eliza: Please, use letters. I am human, after all.')
            #加上换行和前缀
            response += "\nYou: "
        else:
            response = generate_response(in_str, script, general_script['substitutions'], memory_stack, memory_inputs)

        #获取用户的下次输入
        in_str = input(response)
        in_str_l = in_str.lower()

    print("Eliza: Goodbye.\n")

if __name__=="__main__":
   main()
