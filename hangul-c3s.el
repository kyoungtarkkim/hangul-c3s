;; -*- lexical-binding: t -*-
;; hangul-c3s.el --- Korean Hangul input method, Cham Shin Sebulshik S

;; Author: Kyoung-Tark Kim(김경탁), kyoungtarkkim@gmail.com
;; Keywords: multilingual, input method, Korean, Hangul, Cham Shin Sebulshik S
;; Version: 1.0 (Nov. 6, 2024)

;; The major portions of this code are modified or derived from `hangul.el'.
;; This code is also inspired by `https://github.com/demokritos/hangul-s3p2'.
;; For the original Cham Shin Sebulshik and Cham Shin Sebulshik D layout,
;; see `https://doc9107.tistory.com/67'.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;; Commentary:
;; The goal of this code is to implement a new Hangul input method into Emacs
;; using the following automata:
;; Hangul Cham Shin Sebulshik S (참신세벌식S, where `S' stands for `Semoi').

;; Cham Shin Sebulshik S is a modified version of Cham Shin Sebulshik D,
;; specifically adjusted for a choseong layout resembling the Semoi layout,
;; which was my favorite Sebulshik layout for a long time (about 10 years?).

;; I made a special effort to write clear code and detailed explanations 
;; so that other people can use this code as a base reference to develop their
;; own desired Sebulshik keyboard layout.

;; In this code, `key', `char', and `charx' refer to:
;; 1. key: ASCII code (from 0 to 127) provided by Emacs input.
;; 2. char: Hangul Jamo index.
;; 3. charx: Unicode for Hangul Jamo.
;; For `char' and `charx', see the table of Hangul Jamo indices below (Table 1).
;; For the printable ASCII codes (from 33 to 126) see the Table 2 below.

;; If you find any improvements or errors in this code,
;; I would appreciate it if you could email me (to: kyoungtarkkim@gmail.com).


;; Code:

(require 'quail)
(require 'hanja-util)

;; Table 1. The Hangul Jamo indicies and Unicodes
;; +------------+------------+------------+------------+
;; | Base index | cho (+0)   |jung (+100) |jong (+1000)|
;; +------------+------------+------------+------------+
;;   1            ᄀ ?\x1100    ᅡ ?\x1161   ᆨ ?\x11a8  
;;   2            ᄁ ?\x1101    ᅢ ?\x1162   ᆩ ?\x11a9  
;;   3            ᄂ ?\x1102    ᅣ ?\x1163   ᆪ ?\x11aa  
;;   4            ᄃ ?\x1103    ᅤ ?\x1164   ᆫ ?\x11ab  
;;   5            ᄄ ?\x1104    ᅥ ?\x1165   ᆬ ?\x11ac  
;;   6            ᄅ ?\x1105    ᅦ ?\x1166   ᆭ ?\x11ad  
;;   7            ᄆ ?\x1106    ᅧ ?\x1167   ᆮ ?\x11ae  
;;   8            ᄇ ?\x1107    ᅨ ?\x1168   ᆯ ?\x11af  
;;   9            ᄈ ?\x1108    ᅩ ?\x1169   ᆰ ?\x11b0  
;;   10           ᄉ ?\x1109    ᅪ ?\x116a   ᆱ ?\x11b1  
;;   11           ᄊ ?\x110a    ᅫ ?\x116b   ᆲ ?\x11b2  
;;   12           ᄋ ?\x110b    ᅬ ?\x116c   ᆳ ?\x11b3  
;;   13           ᄌ ?\x110c    ᅭ ?\x116d   ᆴ ?\x11b4  
;;   14           ᄍ ?\x110d    ᅮ ?\x116e   ᆵ ?\x11b5  
;;   15           ᄎ ?\x110e    ᅯ ?\x116f   ᆶ ?\x11b6  
;;   16           ᄏ ?\x110f    ᅰ ?\x1170   ᆷ ?\x11b7  
;;   17           ᄐ ?\x1110    ᅱ ?\x1171   ᆸ ?\x11b8  
;;   18           ᄑ ?\x1111    ᅲ ?\x1172   ᆹ ?\x11b9  
;;   19           ᄒ ?\x1112    ᅳ ?\x1173   ᆺ ?\x11ba  
;;   20                        ᅴ ?\x1174   ᆻ ?\x11bb  
;;   21                        ᅵ ?\x1175   ᆼ ?\x11bc  
;;   22                                     ᆽ ?\x11bd  
;;   23                                     ᆾ ?\x11be  
;;   24                                     ᆿ ?\x11bf  
;;   25                                     ᇀ ?\x11c0  
;;   26                                     ᇁ ?\x11c1  
;;   27                                     ᇂ ?\x11c2  
;; +------------+------------+------------+------------+
;; Thus, there are a total of 19 * 21 * (27 +1) = 11,172 modern Hangul syllables.
;; The Unicode formula for Hangul combination is:
;; Hangul unicode = #xac00 + (21*28* (cho_index -1)) + (28* (jung_index -101))
;;                         + (jong_index -1000)
;; e.g., 힣's unicode = ?\xd7a3 = ?\xac00 + (588* 18) + (28* 20) + 27
;; Note: ㅎ(cho),ㅣ(jung),ㅎ(jong) have indices 19, 121, and 1027, respectively.
;;       (These indices are not standard; they are temporarily assigned by me.)


;; The combination rule for choseong in hangul-c3s is:
;; ㄲ=ㄱ(k)+ㄱ(k)=ㄱ(k)+ㅇ(j)=ㅇ(j)+ㄱ(k)
;; ㅋ=ㄱ(k)+ㅎ(h)=ㅎ(h)+ㄱ(k)
;; ㄸ=ㄷ(i)+ㄷ(i)=ㄷ(i)+ㅇ(j)=ㅇ(j)+ㄷ(i)
;; ㅌ=ㄷ(i)+ㅎ(h)=ㅎ(h)+ㄷ(i)
;; ㅃ=ㅂ(o)+ㅂ(o)=ㅂ(o)+ㅇ(j)=ㅇ(j)+ㅂ(o)
;; ㅍ=ㅂ(o)+ㅎ(h)=ㅎ(h)+ㅂ(o)
;; ㅆ=ㅅ(n)+ㅅ(n)=ㅅ(n)+ㅇ(j)=ㅇ(j)+ㅅ(n)
;; ㅈ=ㅈ(l)+ㅈ(l)=ㅈ(l)+ㅇ(j)=ㅇ(j)+ㅈ(l)
;; ㅊ=ㅈ(l)+ㅎ(h)=ㅎ(h)+ㅈ(l)

;; The combination rule for choseong to make a double jongseong in hangul-c3s is:
;; ㄳ=ㄱ(k)+ㅅ(n)
;; ㄲ=ㄱ(k)+ㄱ(k)
;; ㄵ=ㄴ(n)+ㅈ(l)
;; ㄶ=ㄴ(n)+ㅎ(h)
;; ㄺ=ㄹ(m)+ㄱ(k)
;; ㄻ=ㄹ(m)+ㅁ(y)
;; ㄼ=ㄹ(m)+ㅂ(o)
;; ㄽ=ㄹ(m)+ㅅ(n)
;; ㄾ=ㄷ(i)+ㄹ(m)=ㄹ(m)+ㄷ(i)
;; ㄿ=ㅂ(o)+ㄹ(m)
;; ㅀ=ㄹ(m)+ㅎ(h)
;; ㅄ=ㅂ(o)+ㅅ(n)

;; The combination rule for jongseong in hangul-c3s is:
;; ㅋ=ㄱ(e)+ㅂ(z)=ㅂ(z)+ㄱ(e)
;; ㄲ=ㄱ(e)+ㅁ(a)=ㅁ(a)+ㄱ(e)=ㄱ(e)+ㄱ(e)
;; ㄳ=ㄱ(e)+ㅅ(x)=ㅅ(x)+ㄱ(e)=ㄱ(e)+ㅆ(q)=ㅆ(q)+ㄱ(e)
;; ㄵ=ㄱ(e)+ㄴ(s)=ㄴ(s)+ㄱ(e)
;; ㄶ=ㄴ(s)+ㅎ(c)=ㅎ(c)+ㄴ(s)=ㅇ(d)+ㄴ(s)=ㄴ(s)+ㅇ(d)
;; ㄺ=ㄹ(w)+ㄱ(e)=ㄱ(e)+ㄹ(w)
;; ㄻ=ㄹ(w)+ㅁ(a)=ㅁ(a)+ㄹ(w)
;; ㄼ=ㄹ(w)+ㅂ(z)=ㅂ(z)+ㄹ(w)=ㄴ(s)+ㅁ(a)=ㅁ(a)+ㄴ(s)
;; ㄽ=ㄹ(w)+ㅅ(x)=ㅅ(x)+ㄹ(w)=ㄹ(w)+ㅆ(q)=ㅆ(q)+ㄹ(w)
;; ㄾ=ㄹ(w)+ㅌ(r)=ㅌ(r)+ㄹ(w)
;; ㄿ=ㅇ(d)+ㅁ(a)=ㅁ(a)+ㅇ(d)
;; ㅀ=ㄹ(w)+ㅎ(c)=ㅎ(c)+ㄹ(w)=ㅇ(d)+ㄹ(w)=ㄹ(w)+ㅇ(d)
;; ㅄ=ㅂ(z)+ㅅ(x)=ㅅ(x)+ㅂ(z)


(defconst hangul-c3s-djamo-table ; This is an alist.
    ;; choseong
  '((1 . [1 10 12 19])        ; ㄱ(k) + ㄱ(k),ㅅ(n),ㅇ(j),ㅎ(h)
    (3 . [13 19])             ; ㄴ(u) + ㅈ(l),ㅎ(h)
    (4 . [4 6 12 19])         ; ㄷ(i) + ㄷ(i),ㄹ(m),ㅇ(j),ㅎ(h)
    (6 . [1 4 7 8 10 19])     ; ㄹ(m) + ㄱ(k),ㅁ(y),ㅂ(o),ㅅ(n),ㅎ(h)
    (8 . [6 8 10 12 19])      ; ㅂ(o) + ㄹ(m),ㅂ(o),ㅅ(n),ㅇ(j),ㅎ(h)
    (10 . [10 12])            ; ㅅ(n) + ㅅ(n),ㅇ(j)
    (13 . [13 12 19])         ; ㅈ(l) + ㅈ(l),ㅇ(j),ㅎ(h)
    (12 . [1 4 8 10 13])      ; ㅇ(j) + ㄱ(k),ㄷ(i),ㅂ(o),ㅅ(n),ㅈ(l)
    (19 . [1 4 8 13])         ; ㅎ(h) + ㄱ(k),ㄷ(i),ㅂ(o),ㅈ(l)

    ;; jungseong
    (109 . [101 102 121])   ; ㅗ(.) + ㅏ(f),ㅐ(s),ㅣ(d)
    (114 . [105 106 121])   ; ㅜ(p) + ㅓ(v),ㅔ(e),ㅣ(d)

    ;; jongseong
    (1001 . [1001 1004 1008 1016 1017 1019 1020])
                        ;; ㄱ(e) + ㄱ(e),ㄴ(s),ㄹ(w),ㅁ(a),ㅂ(z),ㅅ(x),ㅆ(q)
    (1004 . [1001 1016 1021 1027])   ; ㄴ(s) + ㄱ(e),ㅁ(a),ㅇ(d),ㅎ(c)
    (1008 . [1001 1016 1017 1019 1020 1021 1025 1027])
                        ;; ㄹ(w) + ㄱ(e),ㅁ(a),ㅂ(z),ㅅ(x),ㅆ(q),ㅇ(d),ㅌ(r),ㅎ(c)
    (1016 . [1001 1004 1008 1021])   ; ㅁ(a) + ㄱ(e),ㄴ(s),ㄹ(w),ㅇ(d)
    (1017 . [1001 1008 1019])        ; ㅂ(z) + ㄱ(e),ㄹ(w),ㅅ(x)
    (1019 . [1001 1008 1017])        ; ㅅ(x) + ㄱ(e),ㄹ(w),ㅂ(z)
    (1020 . [1001 1008])             ; ㅆ(q) + ㄱ(e),ㄹ(w)
    (1021 . [1004 1008 1016])        ; ㅇ(d) + ㄴ(s),ㄹ(w),ㅁ(a)
    (1025 . [1008])                  ; ㅌ(r) + ㄹ(w)
    (1027 . [1004 1008])))           ; ㅎ(c) + ㄴ(s),ㄹ(w)




;; Table 2. Printable ASCII codes (from 32 to 126)
;; Note that 32 (?\x20) is a space, namely " ", so omitted.
;;  +-------+-------+-------+
;;  |  Dec  |  Hex  | Text  |
;;  +-------+-------+-------+
;;  |  33   | ?\x21 |   !   |
;;  |  34   | ?\x22 |   "   |
;;  |  35   | ?\x23 |   #   |
;;  |  36   | ?\x24 |   $   |
;;  |  37   | ?\x25 |   %   |
;;  |  38   | ?\x26 |   &   |
;;  |  39   | ?\x27 |   '   |
;;  |  40   | ?\x28 |   (   |
;;  |  41   | ?\x29 |   )   |
;;  |  42   | ?\x2a |   *   |
;;  |  43   | ?\x2b |   +   |
;;  |  44   | ?\x2c |   ,   |
;;  |  45   | ?\x2d |   -   |
;;  |  46   | ?\x2e |   .   |
;;  |  47   | ?\x2f |   /   |
;;  |  48   | ?\x30 |   0   |
;;  |  49   | ?\x31 |   1   |
;;  |  50   | ?\x32 |   2   |
;;  |  51   | ?\x33 |   3   |
;;  |  52   | ?\x34 |   4   |
;;  |  53   | ?\x35 |   5   |
;;  |  54   | ?\x36 |   6   |
;;  |  55   | ?\x37 |   7   |
;;  |  56   | ?\x38 |   8   |
;;  |  57   | ?\x39 |   9   |
;;  |  58   | ?\x3a |   :   |
;;  |  59   | ?\x3b |   ;   |
;;  |  60   | ?\x3c |   <   |
;;  |  61   | ?\x3d |   =   |
;;  |  62   | ?\x3e |   >   |
;;  |  63   | ?\x3f |   ?   |
;;  |  64   | ?\x40 |   @   |
;;  |  65   | ?\x41 |   A   |
;;  |  66   | ?\x42 |   B   |
;;  |  67   | ?\x43 |   C   |
;;  |  68   | ?\x44 |   D   |
;;  |  69   | ?\x45 |   E   |
;;  |  70   | ?\x46 |   F   |
;;  |  71   | ?\x47 |   G   |
;;  |  72   | ?\x48 |   H   |
;;  |  73   | ?\x49 |   I   |
;;  |  74   | ?\x4a |   J   |
;;  |  75   | ?\x4b |   K   |
;;  |  76   | ?\x4c |   L   |
;;  |  77   | ?\x4d |   M   |
;;  |  78   | ?\x4e |   N   |
;;  |  79   | ?\x4f |   O   |
;;  |  80   | ?\x50 |   P   |
;;  |  81   | ?\x51 |   Q   |
;;  |  82   | ?\x52 |   R   |
;;  |  83   | ?\x53 |   S   |
;;  |  84   | ?\x54 |   T   |
;;  |  85   | ?\x55 |   U   |
;;  |  86   | ?\x56 |   V   |
;;  |  87   | ?\x57 |   W   |
;;  |  88   | ?\x58 |   X   |
;;  |  89   | ?\x59 |   Y   |
;;  |  90   | ?\x5a |   Z   |
;;  |  91   | ?\x5b |   [   |
;;  |  92   | ?\x5c |   \   |
;;  |  93   | ?\x5d |   ]   |
;;  |  94   | ?\x5e |   ^   |
;;  |  95   | ?\x5f |   _   |
;;  |  96   | ?\x60 |   `   |
;;  |  97   | ?\x61 |   a   |
;;  |  98   | ?\x62 |   b   |
;;  |  99   | ?\x63 |   c   |
;;  | 100   | ?\x64 |   d   |
;;  | 101   | ?\x65 |   e   |
;;  | 102   | ?\x66 |   f   |
;;  | 103   | ?\x67 |   g   |
;;  | 104   | ?\x68 |   h   |
;;  | 105   | ?\x69 |   i   |
;;  | 106   | ?\x6a |   j   |
;;  | 107   | ?\x6b |   k   |
;;  | 108   | ?\x6c |   l   |
;;  | 109   | ?\x6d |   m   |
;;  | 110   | ?\x6e |   n   |
;;  | 111   | ?\x6f |   o   |
;;  | 112   | ?\x70 |   p   |
;;  | 113   | ?\x71 |   q   |
;;  | 114   | ?\x72 |   r   |
;;  | 115   | ?\x73 |   s   |
;;  | 116   | ?\x74 |   t   |
;;  | 117   | ?\x75 |   u   |
;;  | 118   | ?\x76 |   v   |
;;  | 119   | ?\x77 |   w   |
;;  | 120   | ?\x78 |   x   |
;;  | 121   | ?\x79 |   y   |
;;  | 122   | ?\x7a |   z   |
;;  | 123   | ?\x7b |   {   |
;;  | 124   | ?\x7c |   |   |
;;  | 125   | ?\x7d |   }   |
;;  | 126   | ?\x7e |   ~   |
;;  +-------+-------+-------+


;; A convenient array of printable ASCII codes (from 33 to 126) is as follows:
;; ! " # $ % & ' ( ) * + , - . /                         (15개)
;; 0 1 2 3 4 5 6 7 8 9                                   (10개)
;; : ; < = > ? @                                          (7개)
;; A B C D E F G H I J K L M N O P Q R S T U V W X Y Z   (26개)
;; [ \ ] ^ _ `                                            (6개)
;; a b c d e f g h i j k l m n o p q r s t u v w x y z   (26개)
;; { | } ~                                                (4개)


;; hangul-c3s keymap.
;; The reason for using Hangul Jamo Unicode (U+1100–U+11FF) in the keymap
;; is simply because choseong, jungseong, and jongseong of Hangul syllable
;; are separated, and there exists a convenient formula for combining them into
;; complete Hangul Characters Unicodes.
(defconst hangul-c3s-keymap
 [?\x21 ?\x22 ?\x23 ?\x24 ?\x25 ?\x26 ?\x27 ?\x28 ?\x29 ?\x2a ?\x2b ?\x2c ?\x2d ?\x2e ?\x2f

   ?\x30 ?\x31 [?\x32 ?\x11bd] [?\x33 ?\x11ae] [?\x34 ?\x11c1] ?\x35 ?\x36 ?\x37 ?\x38 ?\x39

   ?\x3a ?\x3b ?\x3c ?\x3d ?\x3e ?\x3f ?\x40

   ?\x2018 ?\x25b3 ?\x300a ?\x201c ?\x300e ?\x201d ?\x25cb ?\x2715 ?\x2192 ?\x00b7 ?\x22ef ?\x2015 ?\x119e ?\C-g ?\x3010 ?\x3011 ?\x300c ?\x300f ?\x2019 ?\x25a1 ?\x2190 ?\x300b ?\x300d ?\x3009 ?\x203b ?\x3008

   ?\x5b ?\x5c ?\x5d ?\x5e ?\x5f ?\x60

   [?\x116d ?\x11b7] ?\x1163 [?\x1173 ?\x11c2] [?\x1175 ?\x11bc] [?\x1166 ?\x11a8] [?\x1161 ?\x11bf] ?\x1169 ?\x1112 ?\x1103 ?\x110b ?\x1100 ?\x110c ?\x1105 ?\x1109 ?\x1107 ?\x116e [?\x1172 ?\x11bb] [?\x116e ?\x11c0] [?\x1162 ?\x11ab] [?\x1167 ?\x11be] ?\x1102 ?\x1165 [?\x1174 ?\x11af] [?\x1164 ?\x11ba] ?\x1106 [?\x1168 ?\x11b8]

   ?\x7b ?\x7c ?\x7d ?\x7e])



(defvar hangul-c3s-im-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\d" #'hangul-c3s-delete-backward-char)
    (define-key map [f9] #'hangul-c3s-to-hanja-conversion)
    (define-key map [Hangul_Hanja] #'hangul-to-hanja-conversion)
    map)
  "Keymap for Hangul method.  It is used by all Hangul input methods.")



(defvar hangul-c3s-queue
  (make-vector 6 0))

;; The following variable is necessary for implementing jungseong and jongseong alternation (so called, `申세벌식_첫가끝_갈마들이') in hangul-c3s.
(defvar hangul-c3s-galma-mode 0
"0 means moeum mode, and 1 means jaeum mode.")


;; Beware: We do NOT use Unicode #x1100-#x11FF for printing choseong, jungseong,
;;         and jongseong. This is because these Unicode characters are large,
;;         making them unsuitable for printing. Instead, we use the smaller Hangul
;;         Compatibility Jamo Unicode characters for printing.
;; Example: Compare the sizes of ?\x1100 = ᄀ and ?\x3131 = ㄱ.

;; Table 3. Hangul Compatibility Jamo (i.e., Jaeum, Moeum)
;; +------------+------------+------------+
;; | Index      | Consonants |   Vowels   |
;; +------------+------------+------------+
;;   1            ㄱ ?\x3131    ㅏ ?\x314f
;;   2            ㄲ ?\x3132    ㅐ ?\x3150
;;   3            ㄳ ?\x3133    ㅑ ?\x3151
;;   4            ㄴ ?\x3134    ㅒ ?\x3152
;;   5            ㄵ ?\x3135    ㅓ ?\x3153
;;   6            ㄶ ?\x3136    ㅔ ?\x3154
;;   7            ㄷ ?\x3137    ㅕ ?\x3155
;;   8            ㄸ ?\x3138    ㅖ ?\x3156
;;   9            ㄹ ?\x3139    ㅗ ?\x3157
;;   10           ㄺ ?\x313a    ㅘ ?\x3158
;;   11           ㄻ ?\x313b    ㅙ ?\x3159
;;   12           ㄼ ?\x313c    ㅚ ?\x315a
;;   13           ㄽ ?\x313d    ㅛ ?\x315b
;;   14           ㄾ ?\x313e    ㅜ ?\x315c
;;   15           ㄿ ?\x313f    ㅝ ?\x315d
;;   16           ㅀ ?\x3140    ㅞ ?\x315e
;;   17           ㅁ ?\x3141    ㅟ ?\x315f
;;   18           ㅂ ?\x3142    ㅠ ?\x3160
;;   19           ㅃ ?\x3143    ㅡ ?\x3161
;;   20           ㅄ ?\x3144    ㅢ ?\x3162
;;   21           ㅅ ?\x3145    ㅣ ?\x3163
;;   22           ㅆ ?\x3146
;;   23           ㅇ ?\x3147
;;   24           ㅈ ?\x3148
;;   25           ㅉ ?\x3149
;;   26           ㅊ ?\x314a
;;   27           ㅋ ?\x314b
;;   28           ㅌ ?\x314c
;;   29           ㅍ ?\x314d
;;   30           ㅎ ?\x314e
;; +------------+------------+------------+
(defun hangul-c3s-character (cho jung jong)
  "Convert CHO, JUNG, JONG to the precomposed `Hangul_Syllables' character.
Return a zero-length string if the conversion fails."
  (or
   (decode-char 'ucs
                (if (and (/= cho 0) (/= jung 0))
                    (+ #xac00
                       (* 588 (1- cho))
                       (* 28 (- jung 101))
                       (cond
                        ((/= jong 0) (- jong 1000))
                        (t 0)))
                  (+ #x3130
	                 (cond ((/= cho 0) (+ cho
                                          (cond ((<= cho 2) 0)
                                                ((<= cho 3) 1)
                                                ((<= cho 6) 3)
                                                ((<= cho 9) 10)
                                                (t 11))))
	                       ((/= jung 0) (+ (- jung 100) #x1e))
	                       ((/= jong 0) (+ (- jong 1000)
                                           (cond ((<= jong 1007) 0)
                                                 ((<= jong 1017) 1)
                                                 ((<= jong 1022) 2)
                                                 (t 3))))))))
   ""))




;; Note: As will become clear, the parameter `queues' consists of either
;;       one queue or two queues (and never more than two queues).
(defun hangul-c3s-insert-character (&rest queues)
  "Insert characters generated from QUEUES.
Each queue in QUEUES has the same form as `hangul-c3s-queue'.
Setup `quail-overlay' to the last character."
  (if (and mark-active transient-mark-mode)
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark)))
  (quail-delete-region)
  (let ((first (car queues)))
    (insert
     (hangul-c3s-character
      (hangul-c3s-djamo (aref first 0) (aref first 1))
      (hangul-c3s-djamo (aref first 2) (aref first 3))
      (hangul-c3s-djamo (aref first 4) (aref first 5)))))
  (move-overlay quail-overlay (overlay-start quail-overlay) (point))
  (dolist (queue (cdr queues))
    (insert
     (hangul-c3s-character
      (hangul-c3s-djamo (aref queue 0) (aref queue 1))
      (hangul-c3s-djamo (aref queue 2) (aref queue 3))
      (hangul-c3s-djamo (aref queue 4) (aref queue 5))))
    (move-overlay quail-overlay (1+ (overlay-start quail-overlay)) (point))))


(defun hangul-c3s-djamo (char1 char2)
  "Return the double Jamo index calculated from the arguments.
CHAR1 and CHAR2 are Hangul Jamo indices.
Return `char1' if CHAR1 and CHAR2 can not be combined."
  (let ((char0 (cdr (assoc char1 hangul-c3s-djamo-table))))
    (if char0
        (let ((i (length char0)))
          (catch 'found
            (while (> i 0)
              (if (= char2 (aref char0 (1- i)))
                  (throw 'found i))
              (setf i (1- i))))
          (cond ((= i 0) char1)
                ((and (>= char1 1) (<= char1 19))              ;; cho seong
                 (cond ((= char1 char2) (1+ char1))
                       ((= char1 12) (1+ char2))
                       ((= char2 12) (1+ char1))
                       ((and (= char1 19) (= char2 1)) 16)
                       ((and (= char1 19) (= char2 4)) 17)
                       ((and (= char1 19) (= char2 8)) 18)
                       ((and (= char1 19) (= char2 13)) 15)
                       ((and (= char2 19) (= char1 1)) 16)
                       ((and (= char2 19) (= char1 4)) 17)
                       ((and (= char2 19) (= char1 8)) 18)
                       ((and (= char2 19) (= char1 13)) 15)
                       ((and (= char1 1) (= char2 10)) 1003)
                       ((and (= char1 3) (= char2 13)) 1005)
                       ((and (= char1 3) (= char2 19)) 1006)
                       ((and (= char1 4) (= char2 6)) 1013)
                       ((and (= char1 6) (= char2 1)) 1009)
                       ((and (= char1 6) (= char2 4)) 1013)
                       ((and (= char1 6) (= char2 7)) 1010)
                       ((and (= char1 6) (= char2 8)) 1011)
                       ((and (= char1 6) (= char2 10)) 1012)
                       ((and (= char1 6) (= char2 19)) 1015)
                       ((and (= char1 8) (= char2 10)) 1018)
                       ((and (= char1 8) (= char2 6)) 1014)))

                ((and (>= char1 101) (<= char1 121)) (+ char1 i)) ;; jung seong

                ((and (>= char1 1001) (<= char1 1027))         ;; jong seong
                 (cond ((= char1 1001) (cond ((= char2 1001) 1002)
                                             ((= char2 1004) 1005)
                                             ((= char2 1008) 1009)
                                             ((= char2 1016) 1002)
                                             ((= char2 1017) 1024)
                                             ((= char2 1019) 1003)
                                             ((= char2 1020) 1003)))
                       ((= char1 1004) (cond ((= char2 1001) 1005)
                                             ((= char2 1016) 1011)
                                             ((= char2 1021) 1006)
                                             ((= char2 1027) 1006)))
                       ((= char1 1008) (cond ((= char2 1001) 1009)
                                             ((= char2 1016) 1010)
                                             ((= char2 1017) 1011)
                                             ((= char2 1019) 1012)
                                             ((= char2 1020) 1012)
                                             ((= char2 1021) 1015)
                                             ((= char2 1025) 1013)
                                             ((= char2 1027) 1015)))
                       ((= char1 1016) (cond ((= char2 1001) 1002)
                                             ((= char2 1004) 1011)
                                             ((= char2 1007) 1010)
                                             ((= char2 1021) 1014)))
                       ((= char1 1017) (cond ((= char2 1001) 1024)
                                             ((= char2 1008) 1011)
                                             ((= char2 1019) 1018)))
                       ((= char1 1019) (cond ((= char2 1001) 1003)
                                             ((= char2 1008) 1012)
                                             ((= char2 1017) 1018)))
                       ((= char1 1020) (cond ((= char2 1001) 1003)
                                             ((= char2 1008) 1012)))
                       ((= char1 1021) (cond ((= char2 1004) 1006)
                                             ((= char2 1008) 1015)
                                             ((= char2 1016) 1014)))
                       ((= char1 1025) (cond ((= char2 1008) 1013)))
                       ((= char1 1027) (cond ((= char2 1004) 1006)
                                             ((= char2 1008) 1015)))))
                (t char1)))
      char1)))




;; The following def-substitution is a combined version of
;; `hangul3-input-method-cho',
;; `hangul3-input-method-jung',
;; and `hangul3-input-method-jong'
;; in the original `hangul.el'.
(defsubst hangul-c3s-input-method-individual (char)
  "Store Hangul Jamo index CHAR in `hangul-c3s-queue' for choseong, jungseong,
 and jongseong."
  (cond
   ((and (>= char 1) (<= char 19))   ;; for choseong
    (let (temp)
      (if (cond ((and (zerop (aref hangul-c3s-queue 0))
                      (zerop (aref hangul-c3s-queue 4)))
                 (aset hangul-c3s-queue 0 char))
                ((and (zerop (aref hangul-c3s-queue 1))
                      (zerop (aref hangul-c3s-queue 2))
                      (zerop (aref hangul-c3s-queue 4))
                      (/= (aref hangul-c3s-queue 0)
                          (setq temp (hangul-c3s-djamo
                                      (aref hangul-c3s-queue 0) char))))
                 (cond
                  ((and (>= temp 1) (<= temp 19))
                   (aset hangul-c3s-queue 1 char))
                  ((and (>= temp 1001) (<= temp 1027))
                   (aset hangul-c3s-queue 0 0)
                   (aset hangul-c3s-queue 4 temp)))))
          (hangul-c3s-insert-character hangul-c3s-queue)
        (hangul-c3s-insert-character hangul-c3s-queue
			                         (setq hangul-c3s-queue (vector char 0 0 0 0 0))))))
   ((and (>= char 101) (<= char 121))   ;; for jungseong
    (if (cond ((and (zerop (aref hangul-c3s-queue 2))
                    (zerop (aref hangul-c3s-queue 4)))
               (aset hangul-c3s-queue 2 char))
              ((and (zerop (aref hangul-c3s-queue 3))
                    (zerop (aref hangul-c3s-queue 4))
                    (/= (aref hangul-c3s-queue 2)
                        (hangul-c3s-djamo (aref hangul-c3s-queue 2) char)))
               (aset hangul-c3s-queue 3 char)))
        (hangul-c3s-insert-character hangul-c3s-queue)
      (hangul-c3s-insert-character hangul-c3s-queue
			                       (setq hangul-c3s-queue (vector 0 0 char 0 0 0)))))
   ((and (>= char 1001) (<= char 1027))   ;; for jongseong
    (if (cond ((and (zerop (aref hangul-c3s-queue 4))
                    (not (zerop (aref hangul-c3s-queue 0)))
                    (not (zerop (aref hangul-c3s-queue 2)))
                    (numberp
                     (hangul-c3s-character
                      (hangul-c3s-djamo
                       (aref hangul-c3s-queue 0)
                       (aref hangul-c3s-queue 1))
                      (hangul-c3s-djamo
                       (aref hangul-c3s-queue 2)
                       (aref hangul-c3s-queue 3))
                      char)))
               (aset hangul-c3s-queue 4 char))
              ((and (zerop (aref hangul-c3s-queue 5))
                    (/= (aref hangul-c3s-queue 4)
                        (hangul-c3s-djamo (aref hangul-c3s-queue 4) char))
                    (numberp
                     (hangul-c3s-character
                      (hangul-c3s-djamo
                       (aref hangul-c3s-queue 0)
                       (aref hangul-c3s-queue 1))
                      (hangul-c3s-djamo
                       (aref hangul-c3s-queue 2)
                       (aref hangul-c3s-queue 3))
                      (hangul-c3s-djamo
                       (aref hangul-c3s-queue 4)
                       char))))
               (aset hangul-c3s-queue 5 char)))
        (hangul-c3s-insert-character hangul-c3s-queue)
      (if (zerop (apply #'+ (append hangul-c3s-queue nil)))
	      (hangul-c3s-insert-character (setq hangul-c3s-queue (vector 0 0 0 0 char 0)))
        (hangul-c3s-insert-character hangul-c3s-queue
			                         (setq hangul-c3s-queue (vector 0 0 0 0 char 0))))))))



(defun hangul-c3s-delete-backward-char ()
  "Delete the previous hangul character by Jaso units."
  (interactive)
  (let ((i 5))
    (while (and (> i 0) (zerop (aref hangul-c3s-queue i)))
      (setq i (1- i)))
    (aset hangul-c3s-queue i 0)
    (if (= i 1)
      (aset hangul-c3s-queue 0 0))
    (cond
     ((<= i 3) (setq hangul-c3s-galma-mode 0))
     ((>= i 4) (setq hangul-c3s-galma-mode 1))))
  (if (not (zerop (apply #'+ (append hangul-c3s-queue nil))))
      (hangul-c3s-insert-character hangul-c3s-queue)
    (setq hangul-c3s-galma-mode 0)
    (delete-char -1)))



(defun hangul-c3s-to-hanja-conversion ()
  "Convert the previous hangul character to the corresponding hanja character.
When a Korean input method is off, convert the following hangul character."
  (interactive)
  (let ((echo-keystrokes 0)
        delete-func
        hanja-character)
    (if (and (overlayp quail-overlay) (overlay-start quail-overlay))
        (progn
	      (setq hanja-character (hangul-to-hanja-char (preceding-char)))
	      (setq delete-func (lambda () (delete-char -1))))
      (setq hanja-character (hangul-to-hanja-char (following-char)))
      (setq delete-func (lambda () (delete-char 1))))
    (when hanja-character
      (funcall delete-func)
      (insert hanja-character)
      (setq hangul-c3s-queue (make-vector 6 0))
      (if (and (overlayp quail-overlay) (overlay-start quail-overlay))
	      (move-overlay quail-overlay (point) (point))))))




;; Support function for `hangul-c3s-input-method'. Actually, this
;; function handles the Hangul-c3s. KEY is an entered key 
;; code (ASCII num.) used for looking up `hangul-c3s-keymap'."
(defun hangul-c3s-input-method-internal (key)
  (let ((charx (aref hangul-c3s-keymap (- key 33))))
    (if (vectorp charx) (setq charx (aref charx hangul-c3s-galma-mode)))
    (cond ((and (>= charx #x1100) (<= charx #x1112)) ;; choseong
           (setq hangul-c3s-galma-mode 0)
           (hangul-c3s-input-method-individual (- charx #x1100 -1)))
          ((and (>= charx #x1161) (<= charx #x1175)) ;; jungseong
           (if (/= key ?p)
               (progn
                 (setq hangul-c3s-galma-mode 1)
                 (hangul-c3s-input-method-individual (- charx #x1161 -101)))
             (setq hangul-c3s-galma-mode 0)
             (hangul-c3s-input-method-individual 114)))
          ((= charx #x11bd)                          ;; number 2
           (setq hangul-c3s-galma-mode 0)
           (if (zerop (aref hangul-c3s-queue 4))
               (hangul-c3s-input-method-individual 1022)
             (setq hangul-c3s-queue (make-vector 6 0))
             (insert ?2)
             (move-overlay quail-overlay (point) (point))))
          ((= charx #x11ae)                          ;; number 3
           (setq hangul-c3s-galma-mode 0)
           (if (zerop (aref hangul-c3s-queue 4))
               (hangul-c3s-input-method-individual 1007)
             (setq hangul-c3s-queue (make-vector 6 0))
             (insert ?3)
             (move-overlay quail-overlay (point) (point))))
          ((= charx #x11c1)                          ;; number 4
           (setq hangul-c3s-galma-mode 0)
           (if (zerop (aref hangul-c3s-queue 4))
               (hangul-c3s-input-method-individual 1026)
             (setq hangul-c3s-queue (make-vector 6 0))
             (insert ?4)
             (move-overlay quail-overlay (point) (point))))
          ((and (>= charx #x11a8) (<= charx #x11c2)) ;; jongseong
           (setq hangul-c3s-galma-mode 1)
           (hangul-c3s-input-method-individual (- charx #x11a8 -1001)))
          ((and (= charx ?.) (zerop hangul-c3s-galma-mode) (not (zerop (aref hangul-c3s-queue 0))) (zerop (aref hangul-c3s-queue 2)))  ;; period 
           (setq hangul-c3s-galma-mode 0)
           (hangul-c3s-input-method-individual 109))
          ((= charx ?\C-g)                           ;; Shift + n
           (setq hangul-c3s-galma-mode 0)
           (setq hangul-c3s-queue (make-vector 6 0))
           (funcall #'keyboard-quit))
          (t
           (setq hangul-c3s-galma-mode 0)
           (setq hangul-c3s-queue (make-vector 6 0))
           (insert charx)
           (move-overlay quail-overlay (point) (point))))))



(defun hangul-c3s-input-method (key)
  "Hangul-c3s input method."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
	      (echo-keystrokes 0)
	      (help-char nil))
      (setq hangul-c3s-galma-mode 0)
      (setq hangul-c3s-queue (make-vector 6 0))
      (hangul-c3s-input-method-internal key)
      (unwind-protect
	      (catch 'exit-input-loop
	        (while t
	          (let* ((seq (read-key-sequence nil))
		             (cmd (lookup-key hangul-c3s-im-keymap seq))
		             key)
		        (cond ((and (stringp seq)
			                (= 1 (length seq))
			                (setq key (aref seq 0))
			                (and (>= key 33) (< key 127)))
		               (hangul-c3s-input-method-internal key))
		              ((commandp cmd)
                       (setq hangul-c3s-galma-mode 0)
		               (call-interactively cmd))
		              (t
                       (setq hangul-c3s-galma-mode 0)
		               (setq unread-command-events
                             (nconc (listify-key-sequence seq)
                                    unread-command-events))
		               (throw 'exit-input-loop nil))))))
	    (quail-delete-overlays)))))



;; Text shown by describe-input-method.  Set to a proper text by
;; hangul-input-method-activate.
(defvar-local hangul-c3s-input-method-help-text nil)


(defun hangul-c3s-input-method-activate (_input-method func help-text &rest _args)
  "Activate Hangul input method INPUT-METHOD.
FUNC is a function to handle input key.
HELP-TEXT is a text set in `hangul-c3s-input-method-help-text'."
  (setq deactivate-current-input-method-function #'hangul-c3s-input-method-deactivate
	describe-current-input-method-function #'hangul-c3s-input-method-help
	hangul-c3s-input-method-help-text help-text)
  (quail-delete-overlays)
  (if (eq (selected-window) (minibuffer-window))
      (add-hook 'minibuffer-exit-hook #'quail-exit-from-minibuffer))
  (setq-local input-method-function func))

(defun hangul-c3s-input-method-deactivate ()
  "Deactivate the current Hangul input method."
  (interactive)
  (unwind-protect
      (progn
        (quail-hide-guidance)
        (quail-delete-overlays)
        (setq describe-current-input-method-function nil))
    (kill-local-variable 'input-method-function)))

(defun hangul-c3s-input-method-help ()
  "Describe the current Hangul input method."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ hangul-c3s-input-method-help-text)))



;; From old hangul.el
(defsubst symbol+ (&rest syms)
  "Concatenate symbols"
  (intern (mapconcat 'symbol-name syms "")))

;; From old hangul.el
(defmacro hangul-register-input-method (package-name language input-method-func package-title package-description package-help)
  "Define input method activate function, deactivate function, help function,
and register input method"
  (let ((activate-func (symbol+ input-method-func '-activate))
        (deactivate-func (symbol+ input-method-func '-deactivate))
        (help-func (symbol+ input-method-func '-help)))
    `(progn
       (defun ,activate-func (&optional arg)
         (if (and arg
                  (< (prefix-numeric-value arg) 0))
             (unwind-protect
                 (progn
                   (quail-hide-guidance)
                   (quail-delete-overlays)
                   (setq describe-current-input-method-function nil))
               (kill-local-variable 'input-method-function))
           (setq deactivate-current-input-method-function ',deactivate-func)
           (setq describe-current-input-method-function ',help-func)
           (quail-delete-overlays)
           (if (eq (selected-window) (minibuffer-window))
               (add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
           (set (make-local-variable 'input-method-function)
                ',input-method-func)))
       (defun ,deactivate-func ()
         (interactive)
         (,activate-func -1))
       (defun ,help-func ()
         (interactive)
         (with-output-to-temp-buffer "*Help*"
           (princ ,package-help)))
       (register-input-method ,package-name
                              ,language
                              ',activate-func
                              ,package-title
                              ,package-description))))


(hangul-register-input-method
 "korean-hangul-c3s"
 "UTF-8"
 hangul-c3s-input-method
 "한3참S"
 "Hangul Cham Shin Sebulshik S Input Method"
 "Input method: korean-hangul-c3s (mode line indicator:한3참S)\n\nHangul Cham Shin Sebulshik S input method.")


(provide 'hangul-c3s)

;;; hangul-c3s.el ends here
