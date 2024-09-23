time_TM
================
2022-11-07

## Disclaimer

It was my HW for discourse course in bachelor. I don’t like my code very
much but to make analysis as full as I can now, I will add this file
into the folder also. It was devoted to topic modelling of the book. I
divided the novel into three books manually and made .csv file in
google-sheets (well, now I of course can do smth wiser, but still).
Preprocessing is done with tidytext and textstem packages, topic
modelling – with topicmodels and ldatuning (to find out the optimal
number of topics).

Загрузка пакетов и обработка переменной chapter (изначально она numeric,
чтобы было удобнее нумеровать главы в таблице во время подготовки
данных). Датасет загружали через environment -\> Import Dataset -\> From
Text (readr) -\> расположение файла на устройстве.

``` r
library(readr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(stopwords)
library(topicmodels)
library(textstem)
library(ldatuning)

Hard_Times = read.csv("C:/Users/yanak/Downloads/Hard_Times.csv")
Hard_Times$Book = as.character(Hard_Times$Book)
```

Лемматизация

``` r
tidy_text = Hard_Times %>%
unnest_tokens(word, Text) %>%
anti_join(stop_words)  %>%
mutate(lemma = textstem::lemmatize_words(word))
```

    ## Joining with `by = join_by(word)`

Частотность лемм по книгам

``` r
word_counts = tidy_text %>%
count(Book, lemma, sort = TRUE) %>%
ungroup()

word_counts = word_counts %>%
filter(n < quantile(word_counts$n, 0.99))
```

Матрица книга-лемма

``` r
times_dtm = word_counts %>% # so, tm is based just on word frequencies
cast_dtm(Book, lemma, n)
```

Определение оптимального количества топиков для моделирования (да, это
считается долго…) \[Murzintcev, 2020\]

``` r
topics_num = FindTopicsNumber(
times_dtm,
topics = seq(from = 2, to = 15, by = 1),
metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
method = "Gibbs",
control = list(seed = 77),
mc.cores = 2L,
verbose = TRUE
)
```

    ## fit models... done.
    ## calculate metrics:
    ##   Griffiths2004... done.
    ##   CaoJuan2009... done.
    ##   Arun2010... done.
    ##   Deveaud2014... done.

``` r
FindTopicsNumber_plot(topics_num)
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## ℹ The deprecated feature was likely used in the ldatuning package.
    ##   Please report the issue at <https://github.com/nikita-moor/ldatuning/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Hard_Times_TM_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Моделирование (LDA) с учётом оптимального кол-ва топиков. Больше всего
нам подойдёт 6 топиков. Посмотрим, что получается, с помощью
визуализации.

``` r
times_lda = LDA(times_dtm, k = 6, control = list(seed = 12345))

times_topics = tidy(times_lda, matrix = "beta")

times_top_terms = times_topics %>%
group_by(topic) %>%
slice_max(beta, n = 10) %>%
ungroup() %>%
arrange(topic, -beta)

times_top_terms %>%
mutate(term = reorder_within(term, beta, topic)) %>%
ggplot(aes(beta, term, fill = factor(topic))) +
geom_col(show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
scale_y_reordered()
```

![](Hard_Times_TM_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> So,
topics 1 and 4 belong to Book 1; 2 and 5 - to Book 2 and the last two -
to Book 3. Talking about the most frequent words, I can say the follows:

- Book 1 is devoted to the city, where everything takes place
  (Coketown); peoples’ interaction (lie - trust, speak, call, love);
  buldings (home, door) and smth diasppointing (cry, poor, lie). I guess
  run and child are binded (because of Sissy Jupe, who was a child and
  had huge problems).

- Book 2. There are vernacular forms of words (ha, thou, ma’am) which
  were used by Stephen Blackpool - one of the main figure at this part
  of the novel. He couldn’t marry Rachel because he was already married
  on the disgusting woman. Poor people couldn’t divorce in Victorian
  England.

- Book 3 tells us about the lady who brought up a son. This son became
  then gentleman and imagined a story that his mom had left him and he
  had reached everything by himself. It is a lie, of course, and his mom
  was waiting everyday near his house, looking at his life, rise and
  falls. No one knew who is this lady until the end of the book.

Though, I’m not really sure whether I could understand these topics if I
didn’t read the book earlier :)
