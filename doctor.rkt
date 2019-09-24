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

; получение значения pairs по ключу или самого ключа, если его нет
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

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-ex2 replacement-pairs lst)
    (let loop ((lst lst) (res '())) ; стартуем с пустым списком в качестве результата
      (if (null? lst) ; если прошли исходный список
          (reverse res) ; реверсим результат, ибо добавляли в начало
          (loop (cdr lst) (cons (replace replacement-pairs (car lst)) res)) ; иначе добавляем в список результат замены и переходим к следующему элементу списка
      )
    )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs через map
(define (many-replace-map replacement-pairs lst)
  (map (lambda (x) (replace replacement-pairs x)) lst) ; для каждого элемента списка выполняем замену
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