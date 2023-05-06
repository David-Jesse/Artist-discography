;; Creating a smart contract that replicates the data structure required to make a streaming service
;; Title: artist-discography
;; Contract that helps an artiste securely store all their catalogue (discography -> album -> deluxe-album -> Track -> Eps -> Music-videos -> Live-album -> Producer -> Record-Label -> Feature -> Featured)

;; Discography
;; An artiste discography is a comprehensive list of all the commmmercially released recordings made by a musician over the course of their career.
;; The artist or admin can start a discography & can add/remove from the disscography.

;; Album
;; An album is a collection of songs or instrumental compositions that are released together as a single cohesive body of work.

;; Track
;; A music track, also known as a song, is a single musical composition that is typically three to five minutes in length and is intended to be listened to as a standalone piece of music.

;; Ep
;; An EP, short for "extended play," is a musical recording that contains more tracks than a single, but fewer tracks than a full-length album.

;; Music-video
;; A music video is a short film or video that accompanies a song or musical composition.

;; Live-album
;; A live album is a musical recording that captures a live performance of an artist or band. 

;; Producer
;; A music producer is a professional who oversees and manages the process of creating a musical recording, typically in a studio setting.

;; Record-label
;; A record label is a company that specializes in producing, marketing, and distributing recorded music.

;;  Features
;; Features made by the artist on an album or a single track.

;; Featured
;; Playlist an artist is featured in.


;;;;;;;;;;;;;;;;;;;;;;
;; Cons, Vars, Maps ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Admins list of principals
(define-data-var admin (list 4 principal) (list tx-sender))

;; Map that keeps track of a single track
(define-map single-track {artist: principal, track-id: uint} {title: (string-ascii 26), duration: uint, featured: (optional principal), producer: (string-ascii 26)})

;; Map that keeps track of a discography
(define-map discography principal (list 30 uint))

;; Map that keeps track of an album
(define-map album {artist: principal, album-id: uint}
    {
        title: (string-ascii 26),
        tracks: (list 100 uint),
        featured: (optional principal),
        height-published: uint,
        producer: (string-ascii 26)
    }
)

;; Map that keeps track of deluxe album
(define-map deluxe-album {artist: principal, album-id: uint}
    {
        title: (string-ascii 26),
        tracks: (list 100 uint),
        featured: (optional principal),
        height-published: uint,
        producer: (string-ascii 26)
    }
)

;; Map that keeps track of an Ep
(define-map ep-project {artist: principal, ep-id: uint} {
    title: (string-ascii 26),
    tracks: (list 6 uint),
    featured: (optional principal),
    producer: (string-ascii 26)
})

;; Map that keeps track of a music videos
(define-map music-video {artist: principal, vid-id: uint} {
    title: (string-ascii 26),
    featured: (optional principal),
    duration: uint,
})

;; Map that tracks Live albums
(define-map live-album {artist: principal, album-id: uint} {
    title: (string-ascii 26),
    tracks: (list 30 uint),
    height-published: uint 
})

;; Map that tracks the record label an artist releases a song, ep or album under
(define-map record-label {artist: principal} {
    album-name: (string-ascii 26),
    label-name: (string-ascii 26),
    year: uint
})

;; Map that keeps track of universal playlist artist is featured in
(define-map featured-in {artist: principal} {
    title: (string-ascii 26),
    songs-featured: uint 
})

;;;;;;;;;;;;;;;;;;;;
;; Read Functions ;;
;;;;;;;;;;;;;;;;;;;;

;; Get track data
(define-read-only (get-track-data (artist principal) (track-id uint)) 
    (map-get? single-track {artist: artist, track-id: track-id})
)

;; Get album-data
(define-read-only (get-album-data (artist principal) (album-id uint)) 
    (map-get? album {artist: artist, album-id: album-id})
)

;; Get deluxe album
(define-read-only (get-deluxe-album (artist principal) (album-id uint)) 
    (map-get? deluxe-album {artist: artist, album-id: album-id})
)

;; Get ep-projects
(define-read-only (get-ep-project (artist principal) (ep-id uint)) 
    (map-get? ep-project {artist: artist, ep-id: ep-id})
)

;; Get music videos
(define-read-only (get-music-vidoes (artist principal) (vid-id uint)) 
    (map-get? music-video {artist: artist, vid-id: vid-id})
)

;; Get live-albums
(define-read-only (get-live-albums (artist principal) (album-id uint)) 
    (map-get? live-album {artist: artist, album-id: album-id})
)

;; Get record-label
(define-read-only (get-record-label (artist principal)) 
    (map-get? record-label {artist: artist})
)

;; Get featured-in-playlist
(define-read-only (get-featured-in (artist principal)) 
    (map-get? featured-in {artist: artist})
)

;; Get discography
(define-read-only (get-artist-discography (artist principal)) 
    (map-get? discography artist)
)


;;;;;;;;;;;;;;;;;;;;;
;; Write Functions ;;
;;;;;;;;;;;;;;;;;;;;;

;; Add a track
;; @desc - Function  that allows a user or admin add a track
;; @params - (title (string-ascii 26) (duration uint) featured-artist (optional) album-id (uint))
(define-public (add-a-single-track (artist principal) (title (string-ascii 26)) (duration uint) (featured (optional principal)) (track-id uint) (producer (string-ascii 26))) 
    (let    
    
        (
            (current-artist-discography (unwrap! (map-get? discography artist) (err "err-no-discography-found")))
            (current-single-track-id (unwrap! (index-of? current-artist-discography track-id) (err "err-track-id")))
            (next-single-track-id (+ u1 current-single-track-id))
        )

            ;; Assert that tx-sender is admin or artist
            (asserts! (or (is-eq tx-sender artist) (is-some (index-of? (var-get admin) tx-sender))) (err "err-not authorized"))

            ;; Assert that duration is less than 600 (10mins)
            (asserts! (< duration u600) (err "err-time-exceeded"))

            ;; Map-set track by appending it to discography
            (ok (map-set single-track {artist: artist, track-id: track-id} {
                title: title,
                duration: duration,
                featured: featured,
                producer: producer
            }))
            
    )
)

;; Add album
;; @desc - A function that allows an artist or admin add a new album or a strat a new discography
(define-public (add-album-or-create-discography-and-add-album (artist principal) (album-id uint) ) 
    (let         
    
        (
            (current-artist-discography (default-to (list ) (map-get? discography artist)))
            (current-album-id (len current-artist-discography))
            (next-album-id (+ u1 current-album-id))
        )

            
            (ok 1)
    )
)


;; (define-map album {artist: principal, album-id: uint}
;;     {
;;         title: (string-ascii 26),
;;         tracks: (list 100 uint),
;;         featured: (optional principal),
;;         height-published: uint,
;;         producer: (string-ascii 26)
;;     }
;; )