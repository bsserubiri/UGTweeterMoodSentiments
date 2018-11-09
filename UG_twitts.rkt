#lang racket
(require data-science)
(require plot)
(require math)
(require json)
(require texpict/mrpict)

;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))
;;; Read in the entire tweet database (3200 tweets from uganda's timeline)
(define tweets (string->jsexpr
                (with-input-from-file "UGTweets.json" (λ () (json-lines->json-array)))))
;;; Remove just the tweet text, source,place, and timestamp from each tweet
;;; hash. Finally, remove retweets.
(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text) (hash-ref x 'source)
                               (hash-ref x 'created_at)(hash-ref x 'place))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))

;; Check the country origin of the tweets 
;;; Label each tweet as coming from iphone, android, or other.
(define tweet-by-type
  (map (λ (x) (list (first x)
                    (cond [(string-contains? (second x) "android") "android"]
                          [(string-contains? (second x) "iphone") "iphone"]
                          [else "other"])))
       t))
;;; Label each tweet as coming from Uganda.
(define UGTweets
  (map (λ (x) (list (first x)
                    (cond [(string? (fourth x)) "Uganda"]
                          [else "other"])))
       t))
;;; Separate tweets by their source (created on iphone vs
;;; android... we aren't interested in tweets from other sources)
(define android (filter (λ (x) (string=? (second x) "android"))  UGTweets))
(define iphone (filter (λ (x) (string=? (second x) "iphone"))  UGTweets))


;; Flatten the list and remove "" empty spaces
(define flatListAndroid (filter (λ (x) (not (equal? x ""))) (flatten android)))
(define flatIphone (filter (λ (x) (not (equal? x ""))) (flatten iphone)))

;;; All words from both sources of tweets
(define JointFile (append flatListAndroid flatIphone))

;; sort the file
;; ;;; Word list from mobile-devices and computer tweets
(define androidWords (sort (sorted-counts flatListAndroid)
                     (λ (x y) (> (second x) (second y)))))
(define iphoneWords (sort (sorted-counts flatIphone)
                     (λ (x y) (> (second x) (second y)))))
(define JointWords (sort (sorted-counts JointFile)
                     (λ (x y) (> (second x) (second y)))))

;;; Now we calculate the log odds ratio of words showing up in tweets
;;; across both platforms
(define (Calc-word-freq i lst)
  (let ([word-frequency (filter (λ (x) (equal? (first x) i)) lst)])
    (if (null? word-frequency) 0 (second (first word-frequency)))))


;;; Sentiment from words coming from either device
(define JointSentiment (filter (λ (x) (second x)) (list->sentiment JointWords #:lexicon
'nrc)))

;;; We calculate the log odds for each affective label from the
;;; sentiment analysis
(define (get-sentiments str)
(map (λ (x)
'(,x
,(log-base (/
(/ (add1 (Calc-word-freq x androidWords)) (add1 (length flatListAndroid)))
(/ (add1 (Calc-word-freq x iphoneWords)) (add1 (length flatIphone))))
#:base 2)))
     ($ (subset JointSentiment (λ (x) (string-contains? x str)) 1) 0)))

;;; Apply the above helper to each affective label
(define sadness (get-sentiments "sadness"))
(define fear (get-sentiments "fear"))
(define anger (get-sentiments "anger"))
(define disgust (get-sentiments "disgust"))
(define surprise (get-sentiments "surprise"))
(define anticipation (get-sentiments "anticipation"))
(define trust (get-sentiments "trust"))
(define joy (get-sentiments "joy"))

;; Helper plotting function
(define (plot-sentiments lst)
  (let* ([n (min 10 (length lst))]
         [top-words (take (sort lst (λ (x y) (> (abs (second x)) (abs (second
                                                                       y))))) n)]
         [androidWords (filter (λ (x) (positive? (second x))) top-words)]
         [iphoneWords (filter (λ (x) (negative? (second x))) top-words)])
    (parameterize ([plot-width 300]
                   [plot-height 400]
                   [plot-x-tick-label-anchor 'right]
                   [plot-x-tick-label-angle 90])
      (plot-pict (list
                  (discrete-histogram androidWords
                                      #:y-min -4
                                      #:y-max 4
                                      #:color "OrangeRed"
                                      #:line-color "OrangeRed")
                  (discrete-histogram (reverse iphoneWords)
                                      #:x-min (length androidWords)
                                      #:y-min -4
                                      #:y-max 4
                                      #:color "LightSeaGreen"
                                      #:line-color "LightSeaGreen"))
                 #:x-label ""
                 #:y-label ""))))


;;; Plot everything together
(vl-append
 (hc-append (ct-superimpose (plot-sentiments sadness) (text "sadness" null 20))
            (ct-superimpose (plot-sentiments fear) (text "fear" null 20))
            (ct-superimpose (plot-sentiments anger) (text "anger" null 20))
            (ct-superimpose (plot-sentiments disgust) (text "digust" null 20)))
 (hc-append (ct-superimpose (plot-sentiments surprise) (text "surprise" null 20))
            (ct-superimpose (plot-sentiments anticipation) (text "anticipation" null 20))
            (ct-superimpose (plot-sentiments trust) (text "trust" null 20))
            (ct-superimpose (plot-sentiments joy) (text "joy" null 20))))


















