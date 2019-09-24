; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; фразы для первого типа генерации ответов
(define FIRST-PHRASES '(
    (you seem to think that)
    (you feel that)
    (why do you believe that)
    (why do you say that)
    (why do you think that)       ; блок 1, задание 1
    (you are saying that)         ; блок 1, задание 1
    (why are you telling me that) ; блок 1, задание 1
    )
)

; фразы для второго типа генерации ответов
(define SECOND-PHRASES '(
    (please go on)
    (many people have the same sorts of feelings)
    (many of my patients have told me the same thing)
    (please continue)
    (don't worry)                 ; блок 1, задание 1
    (cool it)                     ; блок 1, задание 1
    (don't let that distress you) ; блок 1, задание 1
    )
)

; словарь замен лиц
(define PERSON-MAP '(
    (am are)
    (are am)
    (i you)
    (me you)
    (mine yours)
    (my your)
    (myself yourself)
    (you i)
    (your my)
    (yours mine)
    (yourself myself)
    )
)

; группы ответов для четвёртого типа генерации оветов
; каждый элемент списка - список ключевых слов и список списков ответов
(define KEYWORDS-PHRASES '(
    (
        (depressed suicide exams university)
        ( 
            (when you feel depressed, go out for ice cream)
            (depression is a disease that can be treated)
            (maybe you need some rest?)
            (you need to spend more time with family or friends)
        )
    )

    (
        (mother father parents brother sister uncle ant grandma grandpa)
        (
            (tell me more about your * , i want to know all about your *)
            (why do you feel that way about your * ?)
            (are you hate your * ?)
            (are you close with your * ?)
        )
    )

    (
        (university scheme lections education)
        (
            (your education is important)
            (how many time do you spend to learning ?)
            (* is needed you to find a good job in the future)
            (* is important, but sometimes need rest)
        )
    )

    (
         (relashionship boyfriend girlfriend)
         (
             (how long have you been worried about your * ?)
             (is it your first * ?)
             (Do your parents know about your *)
         )
    )

    (
        (money buiseness job work)
        (
            (* is not the main thing in life)
            (do your like your job ?)
            (do not make all money)
        )
    )
)
)

; уникальные ключевые слова
; TODO: по возможности оптимизировать
(define KEYWORDS (foldl (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) '() (foldl (lambda (x y) (append (car x) y)) '() KEYWORDS-PHRASES)))

; Блок 2, задание 5
; основная функция, запускающая "Доктора"
; параметр endname -- имя пациента, на котором необходимо завершить приём
; параметр patients-left -- число оставшихся пациентов
(define (visit-doctor endname patients-left)
    (let ((name (process-visit endname patients-left))) ; обрабатываем визит
        (cond
            ((boolean? name) ; если завершили визиты
                (print '(time to go home)) ; уходим домой
            )
            (else
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop name '()) ; обрабатываем пациента
                (visit-doctor endname (sub1 patients-left)) ; переходим к следующему
            )
        )
    )
)

; Блок 2, задание 5
; обработка визита очередного пациента
; возвращается #f, если пациенты закончились или имя совпало с завершающим
; иначе возвращается имя
(define (process-visit endname patients-left)
    (cond
        ((< patients-left 0) (printf "Invalid patients left\n") #f) ; если некорректное число пациентов, то сообщаем об этом
        ((= patients-left 0) #t) ; если пациенты закончились, то true
        (else
            (let ((name (ask-patient-name))) ; спрашиваем имя
                (if (equal? endname name) ; если совпало с завершающим, то true, иначе само имя
                    #t
                    name
                )
            )
        )
    )
)

; Блок 2, задание 5
; получение имени пользователя
(define (ask-patient-name)
    (begin
        (println '(next!))
        (println '(who are you?))
        (print '**)
        (car (read))
    )
)

; Блок 3, задание 4
; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; параметр user-history -- история ответов пациентов
(define (doctor-driver-loop name user-history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
        (cond 
            ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                (printf "Goodbye, ~a!\n" name)
                (printf "see you next week\n")
            )
            (else (print (reply user-response user-history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                (doctor-driver-loop name (cons user-response user-history))
            )
        )
    )
)

; Блок 3, задание 4
; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response user-history)
    (let*(
            (have-history (not (null? user-history))) ; есть ли история
            (keywords (filter (lambda (x) (ormap (lambda (y) (equal? y x)) KEYWORDS)) user-response)) ; получаем все ключевые слова из фразы
            (have-keywords (not (null? keywords))) ; есть ли хоть одно ключевое слово в списках
            (func-count (+ 2 (if have-history 1 0) (if have-keywords 1 0))) ; количество функций для генерации рандома
            (func-index (random func-count)) ; индекс случайной функции
        )

        (case func-index ; с равной вероятностью выбирается один из двух способов построения ответа
            ((0) (qualifier-answer user-response)) ; 1й способ
            ((1) (hedge))  ; 2й способ
            ((2) (if have-history (history-answer user-history) (keywords-answer user-response keywords))) ; 3й способ (или 4 способ)
            ((3) (keywords-answer user-response keywords))
        )
    )
)
            
; случайный выбор одного из элементов списка lst
(define (pick-random lst)
    (list-ref lst (random (length lst)))
)

; замена лица во фразе          
(define (change-person phrase)
    (many-replace-map PERSON-MAP phrase)
 )

; получение значения pairs по ключу или самого ключа, если его нет
; параметр pairs -- список пар для замен
; параметр key -- ключ, по которому производится замена
(define (replace pairs key)
    (let ((check-key (assoc key pairs))) ; получаем пару, соответствующую ключу
        (if check-key ; если есть такое значение
            (cadr check-key) ; возвращаем его
            key ; иначе возвращаем исходный ключ
        )
    )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
    (cond
        ((null? lst) lst) ; если список пуст, то и возвращать нечего
        (else (cons (replace replacement-pairs (car lst)) (many-replace replacement-pairs (cdr lst)))) ; иначе добавляем результат замены к результату оставшегося списка
    )
)

; Блок 1, задание 2
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-ex2 replacement-pairs lst)
    (let loop ((lst lst) (res '())) ; стартуем с пустым списком в качестве результата
        (if (null? lst) ; если прошли исходный список
            (reverse res) ; реверсим результат, ибо добавляли в начало
            (loop (cdr lst) (cons (replace replacement-pairs (car lst)) res)) ; иначе добавляем в список результат замены и переходим к следующему элементу списка
        )
    )
)

; Блок 1, задание 3
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs через map
(define (many-replace-map replacement-pairs lst)
    (map (lambda (x) (replace replacement-pairs x)) lst) ; для каждого элемента списка выполняем замену
)

; получение списка индексов групп, в которых есть ключевое слово keyword
(define (get-group-indexes keyword)
    (let loop ((lst KEYWORDS-PHRASES) (index 0) (res '()))
        (cond
            ((null? lst) res) ; пройдя по списку, возвращаем результат
            (else
                (loop (cdr lst) (add1 index)
                      (if (ormap (lambda (x) (equal? x keyword)) (car (car lst))) ; если ключ есть в какой-то группе
                          (cons index res) ; то добавляем индекс в список
                          res ; иначе осталвяем без изменений
                      )
                )
            )
        )
    )
)

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
    (append (pick-random FIRST-PHRASES)
        (change-person user-response)
    )
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
    (pick-random SECOND-PHRASES)
)

; Блок 1, задание 4
; 3й споособ
(define (history-answer history)
    (append '(earlier you said that) (change-person (pick-random history)))
)

; Блок 2, задание 6
; 4й способ генерации ответа -- случайный выбор шаблона по ключевым словам
(define (keywords-answer response keywords)
    (let* (
            (keyword (pick-random keywords))
            (indexes (get-group-indexes keyword)) ; индексы групп, в которых есть ключевое слово keyword
            (group (list-ref KEYWORDS-PHRASES (pick-random indexes))) ; случайно выбранная группа
            (template (pick-random(list-ref group 1))) ; случайно выбранный шаблон группы
         )

         ;(map (lambda (x) (if (equal? x '*) keyword x)) template) ; этот вариант поэффективнее будет, но, увы, в задании требуется использовать существующую стратегию замен
         (many-replace-map (list (list '* keyword)) template)
    )
)