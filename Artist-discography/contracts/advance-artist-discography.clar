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
(define-map single-track principal {title: (string-ascii 26), duration: uint, featured: (optional principal)})

;; Map that keeps track of a simple album
(define-map album {artist: principal, album-id: uint}
    {
        title: (string-ascii 26),
        tracks: (list 100 uint),
        height-published: uint,
        producer: (string-ascii 26)
    }
)

;; Map that keeps track of deluxe album
(define-map deluxe-album {artist: principal, album-id: uint}
    {
        title: (string-ascii 26),
        tracks: (list 100 uint),
        height-published: uint,
        producer: (string-ascii 26)
    }
)

;; Map that keeps track of an Ep
(define-map ep-project principal {
    title: (string-ascii 26),
    tracks: (list 6 uint),
    featured: (optional principal),
    producer: (string-ascii 26)
})

;; Map that keeps track of a music-video
(define-map music-video principal {
    title: (string-ascii 26),
    duration: uint,
})

;; Map that tracks Live albums
(define-map live-album {artist: principal, album-id: uint} {
    title: (string-ascii 26),
    tracks: (list 30 uint),
    height-published: uint 
})

;; Map that tracks the producer of a song, Ep, and album
(define-map producer principal {
    title: (string-ascii 26),
    track: (list 30 uint),
})


