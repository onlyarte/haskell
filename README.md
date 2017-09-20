# Haskell programs

<pre>Kyiv-Mohyla Academy
Functional programming 2017
Course projects</pre>

<h1>Homework 1</h1>
На основі допоміжного файлу створити файл, в якому надати  визначення наступних функцій. 
1.	Рекурсивна функція factorial n, котра вираховує факторіал цілого числа n. Передумова - n≥0. 
2.	Функція listSum lx1 lx2, котра додає елементи двох списків цілих чисел і повертає список, складений з сум елементів списків lx1 lx2. Якщо списки мають різну довжину, то коротший розширюється нулями. Наприклад:  
•	listSum [1,2,5] [5, 4,-1] = [6,6,4]
•	listSum [1,2,5] [1] = [2,2,5]
3.	Функція  oddEven lx, котра  міняє місцями значення сусідні парних і непарних елементів списку lx.   Наприклад:
•	oddEven [5,7,6,4,3] = [7,5,4,6,3]
4.	Функція  power3, котра  будує нескінченний список  третьої степені натуральних чисел. Наприклад
•	take 4 power3 = [1, 8, 27, 64]
5.	Функція  toPower3, котра  будує нескінченний список  натуральних степенів числа три. Наприклад:
•	take 4 toPower3 = [3, 9, 27, 81]
6.	Функція  sumPower3 n, котра  обраховує суму ряду  . Можна скористатися функцією sum xs, що рахує суму всіх елементів списку xs.  Наприклад:
•	sumPower3  4 = 120
7.	Функція  sumPower  m n, котра  обраховує суму ряду  , де F(m,i) = mi. Передумова - m≥0. Можна скористатися функцією sum. Наприклад:
•	sumPower 2 5  = 62
8.	Функція  expPart  m n, котра  обраховує суму ряду  , де F(m,i) = mi/i!. Передумова - m≥0. Можна скористатися функціями factorial і sum. Наприклад:
•	expPart 2 5  = 6.266666666666667
9.	Функція factorialsM, котра будує нескінченний список факторіалів. Можна скористатися функцією factorial. Наприклад:
•	take  5 factorialsM  = [ 1, 2, 6, 24, 120]
10.	Функція  frequency lx, що за списком цілих чисел lx, повертає список пар (елемент, частота). Кожна пара визначає елемент із списку lx і частоту його входження в список lx.  Можна скористатися функцією length. Наприклад:
•	frequency [1,1,2,2,1,1] =  [(1,4),(2,2)]

<h1>Homework 2</h1>
1.	Функція sumFl ix, котра вираховує суму цілих чисел – елементів списку ix. Використайте функцію foldl. 
2.	Функція productFr ix, котра вираховує добуток цілих чисел – елементів списку ix. Використайте функцію foldr.
3.	Функція concatFr i1x i2x, котра виконує конкатенацію двох списків цілих чисел i1x і i2x. Функція повинна мати ту  ж семантику, що і оператор (++). Використайте функцію foldr. Наприклад:
•	concatFr [5,1,4] [6,7] = [5,1,4, 6,7]
4.	Функція sortInsert lx, котра сортує список цілих чисел в порядку зростання.  Використайте функцію foldl. Можна побудувати допоміжну функцію insert ix v, що вставляє новий елемент v у впорядкований список ix. Наприклад:  
•	sortInsert [9, 1, 7, 5, 8]  = [1,5, 7, 8, 9]
5.	Функція  findIndices p lx, котра  знаходить індекси тих елементів списку цілих чисел lx, котрі задовольняють предикат p. Індекси елементів починаються з нуля..   Наприклад:
•	findIndices odd [5,7,6,4,3] = [0,1,4]
6.	Функція  allReverse sx, котра  бере список рядків sx  і перевертає список sx і всі рядки, що входять до нього.  Використайте функції map і reverse.Наприклад
•	allReverse [“abc”, “”, “123”, “fg”]  = [“gf”,”321”, “”, “cba”]
7.	Функція noDigits lx, котра вилучає з рядка lx всі десяткові цифри. Використайте функції filter і elem. 
8.	Функція cntGood px v, котра вираховує скільком предикатам зі списку  px задовольняє ціле значення v. Наприклад:
•	cntGood [(>0), odd, (<12), (/=5)] 6 = 3
9.	Функція  triangle, котра  будує нескінченний список  трикутних чисел.  Використайте функцію  scanl або scanl1. Наприклад:
•	take 4 triangle = [1, 3, 6, 10]
10.	Функція  piramid, котра  будує нескінченний список  пірамідальних чисел.  Використайте функцію  scanl або scanl1. Наприклад:
•	take 5 piramid = [1, 5, 14, 30, 55]
11.	Функція factorialsM, котра будує нескінченний список факторіалів. Використати функцію zipWith. Наприклад:
•	take  5 factorialsM  = [1, 2, 6, 24, 120]
