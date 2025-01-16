# hangul-c3s
Emacs용 "참신세벌식S" 한글 내부 입력기
    
## 특정 한글 자판에 대해서 Emacs 내부 입력기가 중요한 구체적이고 일반적인 이유
Emacs 사용자들은 org를 좋아합니다! 끄적 끄적 적고 무엇인가를 계획하고 싶을 때, 한국인이라면 아무래도 영문보다는 한글이 더 편하겠지요...
하지만 만일 Emacs 내부 입력기가 있다면, 한글로 글을 적으면서도 한영 전환 필요없이 Emacs 명령 키조합들을 빨리 빨리 처리할 수 있습니다! (~~EMACS=Esc+Meta+Alt+Contol+Shift~~)
그러니, Emacs 유저들은 자신이 좋아하는 세벌식 Layout을 가진 내부 입력기를 간절히 바랄 수도 있습니다(?).
왜냐하면, Emacs의 기본 내장 입력기는 두벌식, 390, 391 Layout밖에 지원하지 않거든요.
  
## 코드를 만들게 된 동기
Emacs와 관계없이 저는 2014년 정도부터 10년 정도 세모이자판을 애용하다가, 최근(2024년 10월)에 호기심에 의해 참신세벌식D를 써보게 되었습니다.
하지만, (참신세벌식D를 개발하신 개발자분의 의도와는 맞지 않겠지만) 초성입력 부분에 조금 불편함을 느끼게 되어서 참신세벌식D의
초성 부분 layout을 저에게 익숙한 세모이쪽으로 바꾼 것이 참신세벌식S입니다.
      
## 참신세벌식S (S는 세모이 자판의 초성을 사용했음을 의미합니다!)
- 따라서, 중성과 종성은 참신세벌식D의 layout을 따르고 있으며, 초성만 세모이의 layout과 동일합니다.
- 마침표 출력은 마침표키를 이용하여도 되고, p키를 이용하여도 됩니다.
- "p"키와 "["키는 초성을 누른 후 반자동으로 "ㅜ"에 대응되고, "."키와 "/"키는 반자동으로 "ㅗ"에 대응됩니다. ";"키와 "'"키는 역시 반자동으로 상황에 따라 각각 "ㅜ"아 "ㅗ"에 대응되게 되는데, 초성이 "ㅁ", "ㄴ", "ㄷ", "ㅌ", "ㄸ", "ㅂ", "ㅍ", "ㅃ"인 경우 "ㅗ"로 작동하게 되며, 초성이 "ㅎ", "ㅇ", "ㄱ", "ㅋ", "ㄲ", "ㅈ", "ㅊ", "ㅉ", "ㅅ", "ㅆ", "ㄹ"인 경우는 "ㅜ"로 작동됩니다.
    
### 참신세벌식S에서의 손가락 사용시 유의할 점
1. (왼손) 모음 "ㅡ"는 왼손으로 타자할 때, 중지를 사용해서는 속도가 나지 않으며, 반드시 집게 손가락으로 치시는 것을 유념해 두셔야 합니다. 그렇게 해야지만 이어지는 받침들과 자연스러운 연결이 이어집니다.
2. (왼손) "ㅣ(모음)" + "ㄱ(받침)" 과 연결시 "ㅣ"는 왼손 중지로, 받침 "ㄱ"은 왼손 약지로 치시는 것을 연습해 두어야 합니다.
3. (오른손) p키와 마침표를 이용해서 오른손 약지로 "ㅜ"와 "ㅗ"를 사용할 수 있으나, "ㅜ"와 "ㅗ"에 대해서는 오른손 새끼 손가락을 전적으로 활용하시는 것을 권장합니다. 왜냐하면 초성 "ㅂ", "ㅍ", "ㅃ, "ㅈ", "ㅊ", "ㅉ" 등과 연결하여 "ㅗ"와 "ㅜ"를 쳐야 할 경우 이미 오른손 약지를 활용하고 있기 때문에 다시 "ㅗ"와 "ㅜ"를 오른손 약지를 써서 치는 것보다, 이 경우 "ㅗ"와 "ㅜ"는 오른손 새끼 손가락을 활용하는 것이 훨씬 연결성이 좋기 때문입니다. 
  
### 참신세벌식S에서의 겹받침 결합 변화
Original 참신세벌식D와는 다르게 참신세벌식S에서의 겹받침 결합법칙에 다음과 같이 약간의 변화를 두었습니다:  

1. 숫자 2가 받침"ㅈ"에 설정되어 있으므로, 겹받침 "ㄵ"은 ㄴ(s)+ㅈ(2)로 입력하지 않도록 참신세벌식S에서는 막아 놓았습니다.
 따라서, 참신세벌식S에서는 겹받침 "ㄵ"는 ㄵ=ㄱ(e)+ㄴ(s)=ㄴ(s)+ㄱ(e)로 입력하셔야 합니다. 막아 놓은 이유는 "한2"를 입력해야 하는 상황에 "핝"으로 입력되기 때문입니다.
2. 비슷한 이유로, 숫자 4가 받침"ㅍ"에 설정되어 있으므로, 겹받침 "ㄿ"은 ㄹ(w)+ㅍ(4)로 입력하지 않도록 참신세벌식S에서는 막아 놓았습니다.
 따라서, 참신세벌식S에서는 겹받침 "ㄿ"는 ㄿ=ㅇ(d)+ㅁ(a)=ㅁ(a)+ㅇ(d)로 입력하셔야 합니다. 막아 놓은 이유는 "할4"를 입력해야 하는 상황에 "핦"으로 입력되기 때문입니다.
  
- 참고: Original 참신세벌식D에서는 ㄵ=ㄴ(s)+ㅈ(2)=ㅈ(2)+ㄴ(s) 그리고 ㄿ=ㄹ(w)+ㅍ(4)=ㅍ(4)+ㄹ(w)를 허용하고 있습니다!
따라서, "한2"의 경우 참신세벌식D에서는 "한"을 입력한 후, 조합 취소키(Shift+N)을 입력하시고 "2"를 입력하셔야 합니다. 제가 이 부분을 수정한 이유는 이러한 조합 취소키 입력을 최소화 하고자 함입니다.
  
### 참신세벌식S에서의 겹받침 강제 출력 변화
1. 참신세벌식S의 초성 layout(즉, 세모이의 초성 layout)에는 거센소리(ㅋ,ㅌ,ㅍ,ㅊ)가 존재하지 않으므로, "ㄿ"과 같은 겹받침을 강제 출력하고 싶은 경우 "ㅂ(o)+ㄹ(m)"로 입력하시면 됩니다.
- 주의: "ㄹ(m)+ㅂ(o)"은 겹받침 "ㄼ" 출력에 해당됩니다.
2. 비슷한 이유로, "ㄾ"과 같은 겹받침을 출력하고 싶은 경우 "ㄷ(i)+ㄹ(m)" 또는 "ㄹ(m)+ㄷ(i)"로 입력하시면 됩니다.
  
### 참신세벌식S에서의 기호 표현 변화
- 참신세벌식D에서는 Shift+L이 "/(slash)"를 출력합니다. 하지만, 참신세벌식S에서는 Shift+L이 "―"를 출력합니다.  
- 참신세벌식D에서는 Shift+O가 "―"를 출력합니다. 하지만, 참신세벌식S에서는 Shift+O가 "【"를 출력합니다.  
- 참신세벌식D에서는 Shift+P가 ";(semicolon)"을 출력합니다. 하지만, 참신세벌식S에서는 Shift+P가 "】"를 출력합니다.  
  
## Original 참신세벌식D와 세모이 자판
Original 참신세벌식 그리고 참신세벌식D 자판과 세모이 자판의 정보에 대해서는 다음 링크를 참조해 주세요:
  
[https://doc9107.tistory.com/67](https://doc9107.tistory.com/67)
  
[https://blog.naver.com/eekdland](https://blog.naver.com/eekdland)
  
## Original 참신세벌식과 Original 참신세벌식D 자판에 대한 Emacs 한글 내부 입력기
[https://github.com/kyoungtarkkim/hangul-c3](https://github.com/kyoungtarkkim/hangul-c3) 를 참조해 주시길 바라겠습니다!
  
## 사용법 (예)
~/.emacs.d/hangul3/hangul-c3s.el 폴더에 파일을 복사해 두었다고 가정하겠습니다.

그러면, ~/.emacs 파일 또는 ~/.emacs.d/init.el 파일 내에 다음과 같이 적어 두시면 됩니다(만일, doom을 사용하신다면 ~/.doom.d/config.el에 쓰시면 됩니다):
  
```elisp
(add-to-list 'load-path "~/.emacs.d/hangul3")   
(require 'hangul-c3s)   
(setq default-input-method "korean-hangul-c3s")
```
  
참고로, emacs내에서 한글 내부 입력기 변환은 Shift+space 또는 Ctrl+backslash를 통해 할 수 있습니다!
