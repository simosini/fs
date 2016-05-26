(**** MODULI (PARTE 2)  QUEUE   ****)

(**

L'obiettivo e' definire e implementare il modulo Queue 
che implementa una coda generica (modulo parametrico).

Le operazioni che deve fornire il modulo sono:

- put: inserisce un elemento nella coda

- get: toglie un elemento dalla coda;
       se la coda e' vuota, solleva una eccezione
   
La coda va gestita in modalita' LIFO (Last In First Out):
il prossimo elemento da prelevare e' l'ultimo elemento che e' stato inserito.

Una invocazione di put o get non deve modificare la code su cui opera
(code immutabili); va pero'  restituita la coda ottenuta dopo l'operazione

Quindi, put e get devono avere tipi:

*  put   : 'a -> Queue<'a> -> Queue<'a>

   put el q =  coda ottenuta inserendo el in q 

*  get : Queue<'a> -> 'a * Queue<'a>

   get q = (el,q1) dove el e' l'elemento tolto da q e
                   q1 e' la coda ottenuta       

**)    


// -------------  Queue.fsi   --------------------

// signature

module Queue  // nome del modulo
type Queue<'a>  // coda parametrica

val empty : Queue<'a>  
val put   : 'a -> Queue<'a> -> Queue<'a>
val get   : Queue<'a> -> 'a * Queue<'a>

exception EmptyQueue
// eccezione da sollevare se si esegue get su una coda vuota

//--------   END  Queue.fsi  --------------


(*

Naive implementation (file QueueN.fs)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Implementiamo la coda con una lista 

- la coda vuota e' rappresentata dalla lista vuota

- l'operazione get e' immediata

- l'operazione put e' invece costosa in quanto occorre attraversare tutta la lista

NOTA
====

Ricordare che [HR, pag. 151]:


*  the type specified as hidden in the signature
   must be a *tagged type* or a *record type*,
   so we have to add a tag in the type definition

Quindi in  QueueN.fs  *non* si puo' definire

type Queue<'a> = 'a list  //  type abbreviation

che genera un errore in compilazione:

--------------------------------------------------------------------
The type definitions in the signature and implementation are not
compatible because an abbreviation is being hidden by a signature.
The abbreviation must be visible to other CLI languages.
Consider making the abbreviation visible in the signature.
----------------------------------------------------------------------

Occorre usare un costruttore (tag value), ad esempio:       

type Queue<'a> =  Q of  'a list

*)   

// --------- QueueN.fs -----------


// Naive implementation of Queue

module Queue
exception EmptyQueue
type Queue<'a> =  Q of  'a list

let empty = Q [] 
// val empty : Queue<'a>

let rec put el = 

// val put : 'a -> Queue<'a> -> Queue<'a>

let get = function
    | Q [] -> raise EmptyQueue
    | Q (h::t) -> (h, Q t);;  
// val get : Queue<'a> -> 'a * Queue<'a>

// --------- END  QueueN.fs -----------

(***  Uso modulo Queue
      ^^^^^^^^^^^^^^^^

Per verificare la correttezza dell'implementazione,  scrivere  uno script file 
test-queue.fsx in cui si compiono le seguenti operazioni:

a) Definire la coda q1  ottenuta ponendo nella coda vuota gli elementi 3, 5, 10.
b) Estrarre un elemento da q1 e sia q2 la coda ottenuta.
   Notare che l'elemento estratto  e' 3.
c) Estrarre un elemento da q2 e sia q3 la coda ottenuta.
   Notare che l'elemento estratto  e' 5.
d) Aggiungere alla coda q3  gli elementi 15, 20 e sia q4 la coda ottenuta.
   Estrarre tre elementi da q3 e sia q5 la coda ottenuta.
   Notare che gli elementi estratti sono 10, 15 e  20.
   Verificare che q5 e' la coda vuota (provare a estrarre elemento da q5).

NOTE
====

1) Il file test-queue.fsx deve iniziare con

#r "QueueN.dll"   // oppure mettere il path completo
open Queue        // si puo' omettere, ma vanno usati i nomi completi (Queue.empty, Queue.put, ...)


2) Eseguendo i test in  test-queue.fsx, l'implementazione della coda e' nascosta.
Se qualcosa non funziona, e' utile definire i test in QueueN.fs,
in modo che sia visibile la lista che implementa la coda.  

***)     

//----------   test-queue.fsx ------

#r "QueueN.dll";;
open Queue;;


(**  A more efficient implementation (file QueueP.fs)
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nell'implementazione naive, l'inserimento di un elemento e' costosa.
Si puo' migliorare usando la rappresentazione proposta da L.C. Paulson (vedi HR Sez. 7.5).

Una coda qs e' rappresentata da una coppia di liste front e rear:

- front e' la lista dei primi elementi della coda
- rear  e' la lista dei rimanenti elementi della coda, elencati pero' in ordine inverso.

Una coda ammette in generale piu' rappresentazioni.
Ad esempio, la coda

   a ; b ; c           

ammette  quattro rappresentazioni:

1) front = [a ; b ; c]   rear = []

2) front = [a ; b]       rear = [c]

3) front = [a]           rear = [c ; b]

4) front = []            rear = [c ; b ; a]

L'operazione put puo' essere implementata in modo efficiente;
infatti, basta aggiungere un elemento in testa alla lista rear

Ad esempio, se la coda  

   a ; b ; c

e' rappresentata come

front = [a ; b]    rear = [c]

aggiungendo l'elemento d si ottiene

front = [a ; b]    rear = [d; c]
 
che rappresenta la coda

 a ; b ; c ; d


L'operazione get si implementa prendendo il primo elemento di front.
Occorre prestare attenzione al caso in cui la lista front sia vuota.
Ad esempio supponiamo di voler eseguire get sulla coda 

   a ; b ; c 

e che la coda sia rappresentata come
 
 front = []                rear = [c ; b ; a]   // (#)

Conviene travasare tutti gli elementi da rear e front;
ovviamente, poiche' la coda deve essere la stessa,
gli elementi vanno posti in ordine inverso:

 front = [a; b; c]          rear = []          // (##)
 
Notare che (#) e (##) rappresentano la stessa coda.
A questo punto, poiche' la lista front non e' vuota, get si implementa
come prima eliminando il primo elemento di front.

i) Implementare  nel file QueueP.fs il modulo Queue rappresentando una
coda con la coppia di liste front e rear (usare un record con due campi).

ii) Eseguire i test in test-queue.fsx con la nuova implementazione.

*)

//----------    file QueueP.fs ---------------

// Paulson's implementation 

module Queue
exception EmptyQueue

type Queue<'a> = {front: 'a list; rear: 'a list}

// let empty = .....
// val empty : Queue<'a>

// let put ........
// val put : 'a -> Queue<'a> -> Queue<'a>

// let rec get ...
// val get : Queue<'a> -> 'a * Queue<'a>


//----------  END  file QueueP,fs ---------------

(*

Esercizio: ulteriori operazioni
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Aggiungere al modulo Queue le seguenti operazioni
e implementarle in QueueN.fs e QueueP.fs:

*  isEmpty  :  Queue<'a> -> bool

   Controlla se una coda e' vuota

*  toList :  Queue<'a> -> 'a list
  
   Restituisce la lista degli elementi in una coda
   (gli elementi vanno elencati in base all'ordine di inserimento).
   Utile per fare test.
   
* ofList :  'a list ->  Queue<'a>

  Inserisce gli elementi di una lista nella coda vuota

* put_list :  'a list ->  Queue<'a>  -> Queue<'a>

 Inserisce gli elementi di una lista in una coda 

*)   
