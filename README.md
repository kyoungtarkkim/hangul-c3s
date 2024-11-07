# hangul-c3k
Emacs용 참신세벌식K 입력기

## 코드를 만들게 된 동기
10년 정도 세모이 e-2016,e-2017,e-2018 자판을 애용하다가 최근(2024년 10월)에 호기심에 의해 참신세벌식D로 갈아타게 되었습니다.
하지만, (참신세벌식D를 개발하신 개발자분의 의도와는 맞지 않겠지만) 초성입력 부분에 조금 불편함을 느끼게 되어서
초성 부분 layout을 저에게 익숙한 세모이쪽으로 바꾼 것이 참신세벌식K입니다.

## 참신세벌식K
따라서, 중성과 종성은 참신세벌식D의 layout을 따르고 있으며, 초성만 세모이의 layout과 동일합니다.
특히, p키가 모음 조합키 "ㅜ"에 해당하고, 마침표가 갈마들이로써 "ㅗ"로 작동합니다.



## 참신세벌식D와 세모이 자판
Original 참신세벌식D 자판과 세모이 자판의 정보에 대해서는 다음 링크를 참조해 주세요:

[https://doc9107.tistory.com/67](https://doc9107.tistory.com/67)

[https://blog.naver.com/eekdland](https://blog.naver.com/eekdland)




## 사용법 (예)
~/.emacs.d/hangul3/hangul-c3k.el 에 파일을 복사해 두었다고 가정하겠습니다.

그러면, ~/.emacs 파일 또는 ~/.emacs.d/init.el 파일 내에 다음과 같이 적어 두시면 됩니다:

(add-to-list 'load-path "~/.emacs.d/hangul3")   
(require 'hangul-c3k)   
(setq default-input-method "korean-hangul-c3k")


참고로 emacs내에서 한영 변환은 Shift+space 또는 Ctrl + backslash 로 할 수 있습니다!

