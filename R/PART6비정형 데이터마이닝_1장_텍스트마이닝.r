# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 328-344


# 텍스트마이닝


## 1. 데이터 전처리
##    <1> tm 패키지 활용
##    용어
##    Corpus : 텍스트데이터의 정제, 통합, 선택, 변환의 과정을 거친 구조화된 단계로 더 이상 추가적 절차 없이 데이터마이닝 알고리즘에 적용할 수 있는 상태
##    R code
##    tm::VectorSource(text) text : 문서로 변경하고자 하는 텍스트 데이터
##    tm::VCorpus(data) data : VectorSource 실행한 데이터
##    tm::tm_map(x, FUN, ...) FUN : 
##                              1) tolower : 소문자로
##                              2) stemDocument : 어근만 남기기
##                              3) stripWhitespace : 공백제거
##                              4) removePunctuation : 문장부호 제거
##                              5) removeNumbers : 숫자 제거
##                              6) removeWords, "word" : 특정 단어 제거
##                              7) removeWords, stopwords("english") : 불용어 제거
##                              8) PlainTextDocument : TextDocument로 변환

##    example(VCorpus 함수 결과)
library(tm)
data(crude)
summary(crude)[1:6,]

inspect(crude[1]) # 문서의 character 수 등 확인 가능

crude[[1]]$content # 내용 확인

##    example(tm_map 활용)
library(tm)

news <- readLines('C:/cha/기타/R/키워드_뉴스.txt')
news # 10개 뉴스 텍스트

news.corpus <- VCorpus(VectorSource(news)) # 문서화 + Corpus화
class(news.corpus) # "VCorpus" "Corpus"
news.corpus[[1]]$content # 결과 확인 '', . 제거 안됨

clean_txt <- function(txt){
    txt <- tm_map(txt, removeNumbers) # 숫자 제거
    txt <- tm_map(txt, removePunctuation) # 문장부호 제거
    txt <- tm_map(txt, stripWhitespace) # 공백 제거(여러 공백을 하나의 공백으로 변환)
    return(txt)
}

##    example(gsub 활용) -tm_map으로 처리되지 않은 텍스트 처리
txt2 <- gsub("[[:punct:]]", "", news.corpus[[1]]$content)
txt2

# gsub 정규표현식   [[:punct:]] : 특수 문자 / [[:digit:]] : 숫자 / [[A-z]] : 알파벳 / [[:alnum:]] : 영문자/숫자


##    <2> 자연어처리(KoNLP 패키지 활용)
##    형태소 분석 등 자연어 처리 및 텍스트 마이닝 수행 가능
##    일반적으로 SejongDic  사전으로 등록해 사용
##    stemming : 어간 추출(형태학적 분석을 단순화한 버젼, 공통 어간을 가지는 단어를 묶음)

##    R code
##    KoNLP::buildDictionary(ext_dic, data) : 직접 사전에 단어 추가 가능  ext_dic : "woorimalsam", "sejong", "insighter" / data : 추가하고자 하는 단어와 품사가 들어갈 data.frame 혹은 txt파일
##    KoNLP::extraNoun(text) : 명사 추출 함수
##    KoNLP::SimplePos22(text) : 형태소 추출해 22개 품사 태그
##    tm::stemDocument(text) : 공통으로 들어가지 않은 부분 제외
##    tm::stemCompletion(text, dictionary)  : stemming된 단어와 완성을 위한 dictionary 함께 넣으면 가장 기본적 어휘 완성

##    example(KoNLP)
# **** R version 4.1.2 KoNLP 설치
'''
참고 : https://e-datanews.tistory.com/155
install.packages("multilinguer")
library(multilinguer)

# 의존성 패키지 설치
install.packages(c("hash","tau","Sejong","RSQLite","devtools","bit","rex","lazyeval","htmlwidgets","crosstalk",
                    "promises","later","sessioninfo","xopen","bit64","blob","DBI","memoise","plogr","DT","rcmdcheck","rversions"), type ="binary")

# github 버전 설치
install.packages("remotes")

# KoNLP 설치
remotes::install_github('haven-jeon/KoNLP', upgrade="never", INSTALL_opts=c("--no-multiarch"))
'''
library(KoNLP)

useSejongDic() # 세종사전 사용

sentence <- "아버지가 방에 스르륵 들어가신다."
extractNoun(sentence) # 아버지 방 스르륵(명사 x)
buildDictionary(ext_dic="sejong",
                user_dic=data.frame(c('슬그머니'), c('mag')))  # mag : 일반부사 -> 품사태그 https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md : Tag information 참고
extractNoun(sentence) # 아버지 방 (buildDictionary 함수를 통해 슬그머니가 명사가 아님을 명시해 줘 extractNoun 결과 변경)

SimplePos22(sentence) # 단어별 태깅 확인

##    example(tm패키지 & KoNLP)

# tm패키지에서 tm_map()에 들어가는 FUN을 그대로 사용하여 전처리가 가능
clean_txt2<-function(txt){
  txt<-removeNumbers(txt)          #숫자 제거
  txt<-removePunctuation(txt)      #문장부호 제거
  txt<-stripWhitespace(txt)        #공백제거
  txt<-gsub("[^[:alnum:]]"," ",txt) #영숫자, 문자를 제외한 것들을 " "으로 처리
  return(txt)
}

clean.news2<-clean_txt2(news)

Noun.news<-extractNoun(clean.news2)     #각 문서마다 명사가 추출

Noun.news[5]      #푸드와 테크, 스타트와 업을 따로 인식하고, 빅데이터를,이 와 우아한형제들대표 등 명사로 인식 못함.

buildDictionary(ext_dic = "sejong",
                user_dic = data.frame(c(read.table("C:/cha/기타/R/food.txt")))) # 복합명사 등 사용자 정의사전 지정
extractNoun(clean.news2)[5] # 푸드테크, 스타트업, 우아한형제들


##    example(SimplePos22 활용해 형용사 추출하기)
library(stringr)
doc1 <- paste(SimplePos22(clean.news2[[2]]))
doc1 # "첨단/NC", "정보통신기술/NC+에/JC", "AI/F", "등/NB+이/JC" , ...

doc2 <- str_match(doc1, "([가-힣]+)/PA") # 한글 + /PA 인 것들만 찾기
# str_match는 첫 번째 값으로 전체가 완벽하게 맞는 것(ex. 더하/PA) + 정규표현식으로 표현된 것 중 맞는 것(ex. "더하")를 return한다
# 따라서 정확히 형용사만 추출하려면 doc2 결과에서 2번째 열의 값들을 가져오면 된다
doc3 <- doc2[,2]
doc3[!is.na(doc3)] # NA 제외


##    example(stemming)
library(tm)
test<-stemDocument(c('analyze', 'analyzed','analyzing'))
test  # "analyz" "analyz" "analyz"
 
completion <- stemCompletion(test, dictionary = c('analyze','analyzed','analyzing')) # 잘린 단어들을 사전 내 기본 단어로 완성 dictionary 필수
completion # "analyze" "analyze" "analyze"


## 2. TDM(Term-Document Matrix)
##    전처리된 데이터에서 각 문서와 단어 간 사용 여부를 이용해 만든 행렬
##    TDM : 문서마다 등장한 단어의 빈도수를 쉽게 파악

##    R code
##    TDM 생성
##    tm::TermDocumentMatrix(data, control) control : 사전 변경, 가중치 부여 등의 옵션 추가기능 지원
##    TDM 활용 분석 및 시각화
##    findAssocs(data, terms, corlimit) data : TDM / terms : 연관성 확인할 데이터 / corlimit : 최소연관성
##    wordcloud(words, freq, min.freq, random.order, colors, ...) : 시각화   min.freq : 시각화 하려는 단어 최소 빈도


##    example(clean.news2 활용)
clean.news2 # 전처리된 텍스트 데이터
VC.news <- VCorpus(VectorSource(clean.news2)) # tm 함수 활용 위해 Corpus로
TDM.news <- TermDocumentMatrix(VC.news)
dim(TDM.news) # 1011(총 단어 개수) *10(문서 개수)
inspect(TDM.news[1:5,])

# 명사만 추출해 TDM 생성
words <- function(doc){  # 명사 추출 함수
    doc <- as.character(doc)
    extractNoun(doc)
}
TDM.news2 <- TermDocumentMatrix(VC.news, control = list(tokenize=words)) # 형태소 추출을 words 함수 사용해 하겠다
dim(TDM.news2) # 289(총 명사 개수) * 10(문서 개수)
inspect(TDM.news2[1:5,])

# 빈도 체크
tdm2 <- as.matrix(TDM.news2) # 행렬화
tdm3 <- rowSums(tdm2) # 행별 합계(명사별 등장횟수 합계)
tdm4 <- tdm3[order(tdm3, decreasing=T)] # 등장횟수 많은 순으로 정렬
tdm4[1:10] # 데이터 : 57 / 빅데이터 : 38 / 서비스 : 20 ...

# 단어사전 정의 및 TDM 구축
mydict <- c("빅데이터","스마트","인공지능","사물인터넷")
my.news<-TermDocumentMatrix(VC.news, control =list(tokenize=words, dictionary=mydict))
inspect(my.news)


##   example(TDM 분석)
words <- function(doc){  # 명사 추출 함수
    doc <- as.character(doc)
    extractNoun(doc)
}
TDM.news2 <- TermDocumentMatrix(VC.news, control = list(tokenize=words)) # 형태소 추출을 words 함수 사용해 하겠다
findAssocs(TDM.news2, '빅데이터',0.9) # 가맹점 0.92 / 개발자 0.92 / 경쟁이 0.92 ...

##   example(TDM 시각화)
library(wordcloud)
tdm2 <- as.matrix(TDM.news2)
term.freq <- sort(rowSums(tdm2), decreasing=T) # 명사별 등장횟수 합산
head(term.freq,5)
#    데이터   빅데이터     서비스     전문가 브라이틱스
#      57        38        20         15      9
#png(filename="wordcloud.png", width=1000, height=800) # 그림 저장
wordcloud(words=names(term.freq),  # 빈도 계산한 것의 이름만
          freq=term.freq,   # 빈도
          min.freq=5,       # 그림으로 그릴 최소 빈도
          random.order=F,
          scale = c(8,3),
          colors=brewer.pal(8,'Dark2'))
#dev.off() # 그림 저장


## 영어 텍스트 마이닝 예시
library(tm)
data(crude)
summary(crude)[1:6,] 
class(crude) # 이미 Corpus
inspect(crude[1]) # 문서의 character 수 등 확인 가능

crude[[1]]$content # 내용 확인

clean_corpus <- function(txt){
    txt <- tm_map(txt,content_transformer(tolower))
    txt <- tm_map(txt, removeNumbers) # 숫자 제거
    txt <- tm_map(txt, removePunctuation) # 문장부호 제거
    txt <- tm_map(txt, stripWhitespace) # 공백 제거(여러 공백을 하나의 공백으로 변환)
    return(txt)
}
clean_crude <- clean_corpus(crude) # 텍스트 전처리

crudestopwords <- c(stopwords('english'), "reuter", " will", "also", "said", "one", "last")
clean_crude <- tm_map(clean_crude, removeWords, crudestopwords)  # 불용어 처리

clean_crude <- tm_map(clean_crude, stemDocument)  # 불용어 처리

crude_tdm <- TermDocumentMatrix(clean_crude) # TDM
crude_mat <- as.matrix(crude_tdm)
crude_freq <- sort(rowSums(crude_mat), decreasing=T)
d <- data.frame(word = names(crude_freq), freq = crude_freq)

library(wordcloud)
wordcloud(words=names(crude_freq),  # 빈도 계산한 것의 이름만
          freq=crude_freq,   # 빈도
          min.freq=15,       # 그림으로 그릴 최소 빈도
          random.order=F,
          scale = c(8,3),
          colors=brewer.pal(8,'Dark2'))
palete <- brewer.pal(7, "Set3")
wordcloud(names(crude_freq),freq=crude_freq,scale=c(5,1),min.freq=15,colors=palete,random.order=F, random.color=T)



# 형태소 추출
#install.packages("openNLP")
library(openNLP)


extractPOS <- function(x, thisPOSregex) {
    x <- as.String(x)
    wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
    POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
    POSwords <- subset(POSAnnotation, type == "word")
    tags <- sapply(POSwords$features, '[[', "POS")
    thisPOSindex <- grep(thisPOSregex, tags)
    tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
    untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
    untokenizedAndTagged
}

tmp <- lapply(clean_crude, extractPOS, "NN$")
library(stringr)
