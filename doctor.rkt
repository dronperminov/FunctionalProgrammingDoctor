; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '())
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name user-history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response user-history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response user-history))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response user-history)
      (case (random (if (null? user-history) 2 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer user-history)) ; 3й способ
      )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (why do you think that)
                               (you are saying that)
                               (why are you telling me that))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-map '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
			(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
			(yourself myself))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-ex2 replacement-pairs lst)
    (let loop ((lst lst) (res '())) ; стартуем с пустым списком в качестве результата
      (if (null? lst) ; если прошли исходный список
          (reverse res) ; реверсим результат
          (let ((key (assoc (car lst) replacement-pairs))) ; получаем результат поиска ключа в списке пар
             (loop (cdr lst) (cons (if key (cadr key) (car lst)) res)) ; переходим к следующему элементу списка, попутно добавляя в начало res либо значение пары, либо исходный элемент списка
          )
      )
    )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs через map
(define (many-replace-map replacement-pairs lst)
  (map (lambda (x)
         (let ((key (assoc x replacement-pairs))) ; запоминаем результат поиска элемента по ключу
           (if key (cadr key) x) ; если нашли, то заменяем на значение пары, иначе оставляем изначальный элемент списка
         )
       ) lst)
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (don't worry)
                       (cool it)
                       (don't let that distress you)
                       )
         )
)

; 3й споособ
(define (history-answer history)
  (append '(earlier you said that) (change-person (pick-random history)))
)