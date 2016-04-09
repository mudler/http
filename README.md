<i>http</i>
=======
This is code is released only for demonstrative and educational purposes, the developer decline every
responsability on usage.
IT was tested only on CLEAN environment so for every illegal use RESPONSABILITY it's totally <b>DECLINED</b>.
What you do it's only your responsability!

What's that bunch of malformed code?
=======
It's a perl shellcode that connects on irc that i developed long time ago.

I repeat: it's old so if you go down to the road you have been warned.
Old means that, as example login it'ìs encrypted with des, and with modern pc it's easy to crack the password,
or some bug will lay here because testing nice features wasn't really useful at the time.

There were a lot of versions used on the net but this is the first public release of the private part,
since i do not intend to mantain it anymore, if someone want to contribute it's free to send pull
request.

I decided to publish the code as it was so could be analyzed and can be identified for security
purposes as i think that some older versions still runs on internet, also because i receive somethimes mails in my old account
from sysadmins that doesn't know how to remove it: simply kill all the process perl interpreter and it's gone. `pkill -9 perl` would work on some hosts.
You have to kill all perl instances because it forks a lot.

Features:
  - Logging system
  - A lot of DoS attacks
  - Easy bash interface
  - Runs everywhere

Why is(was) different:
  - It doesn't require modules. the unique dependency it's perl itself (and you can defeat that wall
with PAR::Packer or perlcc)
  - Since it doesn't require modules it works on mostly unix/windows systems that has the perl
interpreter, time ago worked also on SunOS, Darwins and *bsd

Password
=======

You have to run crypt.pl to encrypt your password and put inside the code (remember that also it hides on
the systems process and remove itself there will be a copy in the memory)

Usage
=======

You have to setup explicitily the configuration in the file, the reason is obvious the purpose of this
software it's to create a fast dosnet from the ground using other tools to exploit hosts.
The shellcode is designed to have less dependencies as possible and as a matter of fact, if you go down
the code (after the configuration part) you will see ugly pasted code from IO::Select and IO::Socket that
 usually is removed by really strict sysadmins policies about security.

At the top of the script there is the configuration part, you need to specify stuff like your irc server, password,
channels to being on (channel password supported) and optionally c codes to compile to have tcp synflood
(if obviously can gain root access).
There are also autorooting features but links are dead so if you intend to use it maybe you need to
compile local exploits and up on a site.

you can start the bot as `perl [programname]` or `./[programname]` with the right permissions

Once the bot is connected you can query him (or you can log in the channel)  and do a `.login password` .
The bot at start try to determine a nice process to fake.

Commands
======

```
!login password - User Login in to bot.
!logout - User Logout from the bot.
!linux - Alcuni comandi utili sui sistemi Unix-Like.
!system - Informazioni sul sistema operativo.
!exploits - Lista gli ultimi exploit di milw0rm.com.
!portscanner ip - Scanning sulle porte pi� frequenti sull'ip dato.
!nmap ip portainizio portafine - Scanning sulle porte indicate sull'ip dato.
!tracedelete - Cancella i Log.
!join chan - Joina sul canale indicato.
!part chan - Esce dal canale indicato.
!processo proc - Cambia il nome del processo in quello indicato.
#comando - Esegue comando.
!eval eval - Eval di un comando (per utenti esperti).
!rebewt - Riavvio del bot.
!cd dir - Cambia Directory di lavoro.
!nick nuovonick - Cambia il nick del bot.
!raw comando - Invia comandi raw al server.
!adminlist - Lista admin.
!back ip porta  - Backdoor.
!udpflood ip porta tempo - Dossa per tot tempo l'ip indicato sulla porta indicata.
!tcpflood ip porta tempo - Dossa per tot tempo l'ip indicato sulla porta indicata.
!synflood ip ip_source pacchetti porta - Syn Flood.
!download url nomefile - Scarica il l'url e lo salva nel file indicato nella cartella corrente.
!oldudpflood ip pacchetti tempo - Dossa per tot tempo l'ip indicato con la dimenzione di tot pacchetti in Kb.
!httpflood ip tempo - Dossa per tot tempo l'ip indicato.
!mail soggetto mittente destinatario messaggio - Manda una mail con i dati indicati.
!massdeface - MassDeface del sito con la variabile dichiarata prima.
!root - Sono rootabile?.
!massroot - Tentativo di AutoRooTing!.
!scan 1 2 3 4- Scanner Dlink!.
!cran 1 2 3- Scanner Dlink!.
!bran 1 2- Scanner Dlink!.
!exploit - Dlink Exploited!.
!lod 80.x.x.x or 80.3.x.x or 6.4.5.x or x.x.x.x (don't be noob!) - Dlink Scanner !.
!quit - Killa il Bot.
Http5 Version 5.1
```

As you see it's half italian(because i'm) and english(because when you write perl code it's difficult to switch italian ), the code will contain also some bugs (huh i dunno, it's been a while) .
