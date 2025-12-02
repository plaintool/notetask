; *** Inno Setup version 6.5.0+ Romanian messages ***
;
; Translated from English by Dmitry Kann, yktooo at gmail.com
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
LanguageName=Română
LanguageID=$0418
LanguageCodePage=0
RightToLeft=no
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Segoe UI
;WelcomeFontSize=14

[Messages]

; *** Application titles
SetupAppTitle=Instalare
SetupWindowTitle=Instalare - %1
UninstallAppTitle=Dezinstalare
UninstallAppFullTitle=Dezinstalare %1

; *** Misc. common
InformationTitle=Informații
ConfirmTitle=Confirmare
ErrorTitle=Eroare

; *** SetupLdr messages
SetupLdrStartupMessage=Acest program va instala %1 pe computerul dvs. Continuați?
LdrCannotCreateTemp=Fișierul temporar nu poate fi creat. Instalare eșuată.
LdrCannotExecTemp=Nu se poate executa fișierul din directorul temporar. Instalare eșuată.
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nEroare %2: %3
SetupFileMissing=Fișierul %1 lipsește din directorul de instalare. Remediați problema sau obțineți o nouă versiune a programului.
SetupFileCorrupt=Fișierele de instalare sunt corupte. Obțineți o nouă copie a programului.
SetupFileCorruptOrWrongVer=Aceste fișiere de instalare sunt corupte sau incompatibile cu această versiune a programului de instalare. Remediați problema sau obțineți o nouă copie a programului.
InvalidParameter=Linia de comandă conține parametri nevalizi:%n%n%1
SetupAlreadyRunning=Programul de instalare rulează deja.
WindowsVersionNotSupported=Acest program nu acceptă versiunea de Windows instalată pe acest computer.
WindowsServicePackRequired=Acest program necesită %1 Service Pack %2 sau mai recent.
NotOnThisPlatform=Acest program nu va rula pe %1.
OnlyOnThisPlatform=Acest program poate fi rulat numai pe %1.
OnlyOnTheseArchitectures=Acest program poate fi instalat numai pe versiuni de Windows pentru următoarele arhitecturi de procesor:%n%n%1
WinVersionTooLowError=Acest program necesită %1 versiunea %2 sau mai recentă.
WinVersionTooHighError=Acest program nu poate fi instalat pe %1 versiunea %2 sau mai recentă.
AdminPrivilegesRequired=Trebuie să fiți conectat ca administrator pentru a instala acest program.
PowerUserPrivilegesRequired=Trebuie să fiți conectat ca administrator sau ca membru al grupului Power Users pentru a instala acest program.
SetupAppRunningError=Programul de instalare a detectat că %1 rulează.%n%nÎnchideți toate instanțele acum, apoi faceți clic pe OK pentru a continua sau pe Anulare pentru a ieși.
UninstallAppRunningError=Programul de dezinstalare a detectat că %1 rulează.%n%nÎnchideți toate instanțele acum, apoi faceți clic pe OK pentru a continua sau pe Anulare pentru a ieși.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Alegeți modul de instalare
PrivilegesRequiredOverrideInstruction=Alegeți modul de instalare
PrivilegesRequiredOverrideText1=%1 poate fi instalat pentru toți utilizatorii (necesită privilegii de administrator) sau doar pentru contul dvs.
PrivilegesRequiredOverrideText2=%1 poate fi instalat doar pentru contul dvs., sau pentru toți utilizatorii (necesită privilegii de administrator).
PrivilegesRequiredOverrideAllUsers=Instalați pentru &toți utilizatorii
PrivilegesRequiredOverrideAllUsersRecommended=Instalați pentru &toți utilizatorii (recomandat)
PrivilegesRequiredOverrideCurrentUser=Instalați doar pentru &mine
PrivilegesRequiredOverrideCurrentUserRecommended=Instalați doar pentru &mine (recomandat)

; *** Misc. errors
ErrorCreatingDir=Nu se poate crea directorul "%1"
ErrorTooManyFilesInDir=Nu se poate crea un fișier în directorul "%1", deoarece conține prea multe fișiere

; *** Setup common messages
ExitSetupTitle=Ieșire din programul de instalare
ExitSetupMessage=Instalarea nu este completă. Dacă ieșiți acum, programul nu va fi instalat.%n%nPuteți efectua instalarea mai târziu rulând programul de instalare din nou.%n%nIeșiți din programul de instalare?
AboutSetupMenuItem=&Despre programul de instalare...
AboutSetupTitle=Despre programul de instalare
AboutSetupMessage=%1, versiunea %2%n%3%n%n%1 pagina web:%n%4
AboutSetupNote=
TranslatorNote=Traducere română de Dmitry Kann, http://www.dk-soft.org/

; *** Buttons
ButtonBack=< &Înapoi
ButtonNext=&Următorul >
ButtonInstall=&Instalează
ButtonOK=&OK
ButtonCancel=Anulează
ButtonYes=&Da
ButtonYesToAll=Da pentru &toate
ButtonNo=&Nu
ButtonNoToAll=Nu pentru to&ate
ButtonFinish=&Finalizează
ButtonBrowse=&Răsfoire...
ButtonWizardBrowse=&Răsfoire...
ButtonNewFolder=&Creează folder nou

; *** "Select Language" dialog messages
SelectLanguageTitle=Selectați limba de instalare
SelectLanguageLabel=Selectați limba de utilizat în timpul instalării.

; *** Common wizard text
ClickNext=Faceți clic pe Următorul pentru a continua sau pe Anulare pentru a ieși din programul de instalare.
BeveledLabel=
BrowseDialogTitle=Răsfoire pentru a selecta un folder
BrowseDialogLabel=Selectați un folder din listă, apoi faceți clic pe OK.
NewFolderName=Folder nou

; *** "Welcome" wizard page
WelcomeLabel1=Bine ați venit la Asistentul de instalare [name]
WelcomeLabel2=Acest program va instala [name/ver] pe computerul dvs.%n%nEste recomandat să închideți toate celelalte aplicații înainte de a continua.

; *** "Password" wizard page
WizardPassword=Parolă
PasswordLabel1=Această instalare este protejată de parolă.
PasswordLabel3=Vă rugăm să introduceți parola, apoi faceți clic pe Următorul. Parolele fac distincție între litere mari și mici.
PasswordEditLabel=&Parolă:
IncorrectPassword=Parola pe care ați introdus-o este incorectă. Vă rugăm să încercați din nou.

; *** "License Agreement" wizard page
WizardLicense=Acord de licență
LicenseLabel=Vă rugăm să citiți următoarele informații importante înainte de a continua.
LicenseLabel3=Vă rugăm să citiți următorul Acord de licență. Trebuie să acceptați termenii acestui acord înainte de a continua.
LicenseAccepted=Am citit și &accept termenii acordului
LicenseNotAccepted=&Nu accept termenii acordului

; *** "Information" wizard pages
WizardInfoBefore=Informații
InfoBeforeLabel=Vă rugăm să citiți următoarele informații importante înainte de a continua.
InfoBeforeClickLabel=Când sunteți gata să continuați instalarea, faceți clic pe Următorul.
WizardInfoAfter=Informații
InfoAfterLabel=Vă rugăm să citiți următoarele informații importante înainte de a continua.
InfoAfterClickLabel=Când sunteți gata să continuați instalarea, faceți clic pe Următorul.

; *** "User Information" wizard page
WizardUserInfo=Informații utilizator
UserInfoDesc=Vă rugăm să introduceți informațiile dvs.
UserInfoName=&Nume utilizator:
UserInfoOrg=&Organizație:
UserInfoSerial=&Număr de serie:
UserInfoNameRequired=Trebuie să introduceți un nume.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selectați locația destinație
SelectDirDesc=În ce folder doriți să instalați [name]?
SelectDirLabel3=[name] va fi instalat în următorul folder.
SelectDirBrowseLabel=Faceți clic pe Următorul pentru a continua. Dacă doriți să alegeți un folder diferit, faceți clic pe Răsfoire.
DiskSpaceGBLabel=Sunt necesari cel puțin [gb] GB spațiu liber pentru a instala programul.
DiskSpaceMBLabel=Sunt necesari cel puțin [mb] MB spațiu liber pentru a instala programul.
CannotInstallToNetworkDrive=Nu se poate instala pe un disc de rețea.
CannotInstallToUNCPath=Nu se poate instala la o cale UNC.
InvalidPath=Trebuie să specificați o cale completă cu o literă de unitate; de exemplu:%n%nC:\APP%n%sau sub formă UNC:%n%n\\server\share
InvalidDrive=Unitatea sau calea de rețea selectată nu există sau nu este accesibilă. Selectați una diferită.
DiskSpaceWarningTitle=Spațiu insuficient pe disc
DiskSpaceWarning=Instalarea necesită cel puțin %1 KB spațiu liber, dar pe unitatea selectată sunt disponibili doar %2 KB.%n%nDoriți totuși să continuați?
DirNameTooLong=Numele folderului sau calea depășesc lungimea maximă permisă.
InvalidDirName=Numele folderului specificat nu este valid.
BadDirName32=Numele folderului nu poate conține caracterele:%n%n%1
DirExistsTitle=Folderul există
DirExists=Folderul:%n%n%1%n%nexistă deja. Instalați în acest folder oricum?
DirDoesntExistTitle=Folderul nu există
DirDoesntExist=Folderul:%n%n%1%n%nnu există. Doriți să-l creați?

; *** "Select Components" wizard page
WizardSelectComponents=Selectați componente
SelectComponentsDesc=Ce componente trebuie instalate?
SelectComponentsLabel2=Selectați componentele pe care doriți să le instalați; debifați componentele pe care nu doriți să le instalați. Faceți clic pe Următorul când sunteți gata să continuați.
FullInstallation=Instalare completă
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalare compactă
CustomInstallation=Instalare personalizată
NoUninstallWarningTitle=Componente deja instalate
NoUninstallWarning=Programul de instalare a detectat că următoarele componente sunt deja instalate pe computerul dvs.:%n%n%1%n%nDacă debifați aceste componente, ele nu vor fi dezinstalate.%n%nContinuați?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Selecția curentă necesită cel puțin [gb] GB pe disc.
ComponentsDiskSpaceMBLabel=Selecția curentă necesită cel puțin [mb] MB pe disc.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selectați sarcini suplimentare
SelectTasksDesc=Ce sarcini suplimentare ar trebui efectuate?
SelectTasksLabel2=Selectați sarcinile suplimentare pe care doriți ca programul de instalare să le efectueze în timpul instalării [name], apoi faceți clic pe Următorul.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selectați folderul din meniul Start
SelectStartMenuFolderDesc=Unde ar trebui ca programul de instalare să creeze scurtăturile?
SelectStartMenuFolderLabel3=Scurtăturile vor fi create în următorul folder din meniul Start.
SelectStartMenuFolderBrowseLabel=Faceți clic pe Următorul pentru a continua. Dacă doriți să alegeți un folder diferit, faceți clic pe Răsfoire.
MustEnterGroupName=Trebuie să introduceți un nume de folder.
GroupNameTooLong=Numele folderului grupului sau calea depășesc lungimea maximă permisă.
InvalidGroupName=Numele folderului specificat nu este valid.
BadGroupName=Numele folderului nu poate conține caracterele:%n%n%1
NoProgramGroupCheck2=&Nu crea un folder în meniul Start

; *** "Ready to Install" wizard page
WizardReady=Gata de instalare
ReadyLabel1=Programul de instalare este gata să instaleze [name] pe computerul dvs.
ReadyLabel2a=Faceți clic pe "Instalează" pentru a continua instalarea sau pe "Înapoi" dacă doriți să revizuiți sau să modificați orice setare.
ReadyLabel2b=Faceți clic pe Instalează pentru a continua instalarea.
ReadyMemoUserInfo=Informații utilizator:
ReadyMemoDir=Calea destinație:
ReadyMemoType=Tip instalare:
ReadyMemoComponents=Componente selectate:
ReadyMemoGroup=Folder meniu Start:
ReadyMemoTasks=Sarcini suplimentare:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Se descarcă fișiere suplimentare...
ButtonStopDownload=Oprește descărcarea
StopDownload=Sigur doriți să opriți descărcarea?
ErrorDownloadAborted=Descărcarea a fost întreruptă
ErrorDownloadFailed=Eroare la descărcare: %1 %2
ErrorDownloadSizeFailed=Eroare la obținerea dimensiunii: %1 %2
ErrorProgress=Eroare la progres: %1 din %2
ErrorFileSize=Dimensiune fișier incorectă: așteptată %1, primită %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractingLabel=Se extrag fișiere suplimentare...
ButtonStopExtraction=Oprește extragerea
StopExtraction=Sigur doriți să opriți extragerea?
ErrorExtractionAborted=Extragerea a fost întreruptă
ErrorExtractionFailed=Extragerea a eșuat: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Parolă incorectă
ArchiveIsCorrupted=Arhiva este coruptă
ArchiveUnsupportedFormat=Format arhivă neacceptat

; *** "Preparing to Install" wizard page
WizardPreparing=Pregătirea instalării
PreparingDesc=Programul de instalare se pregătește să instaleze [name] pe computerul dvs.
PreviousInstallNotCompleted=Instalarea sau dezinstalarea anterioară nu a fost finalizată. Va trebui să reporniți computerul pentru a finaliza acea instalare.%n%nDupă repornire, rulați din nou Programul de instalare pentru a finaliza instalarea [name].
CannotContinue=Instalarea nu poate continua. Faceți clic pe "Anulare" pentru a ieși.
ApplicationsFound=Următoarele aplicații folosesc fișiere pe care programul de instalare trebuie să le actualizeze. Este recomandat să permiteți programului de instalare să închidă automat aceste aplicații.
ApplicationsFound2=Următoarele aplicații folosesc fișiere pe care programul de instalare trebuie să le actualizeze. Este recomandat să permiteți programului de instalare să închidă automat aceste aplicații. După finalizarea instalării, programul de instalare va încerca să le repornească.
CloseApplications=Închide automat aceste aplicații
DontCloseApplications=Nu închide aceste aplicații
ErrorCloseApplications=Programul de instalare nu a putut închide automat toate aplicațiile. Este recomandat să închideți orice aplicație care utilizează fișierele ce vor fi actualizate înainte de a continua.
PrepareToInstallNeedsRestart=Programul de instalare trebuie să repornească computerul dvs. După repornire, rulați din nou programul de instalare pentru a finaliza instalarea [name].%n%nReporniți acum?

; *** "Installing" wizard page
WizardInstalling=Se instalează...
InstallingLabel=Vă rugăm să așteptați în timp ce [name] este instalat pe computerul dvs.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalizarea Asistentului de instalare [name]
FinishedLabelNoIcons=[name] a fost instalat pe computerul dvs.
FinishedLabel=[name] a fost instalat pe computerul dvs. Puteți lansa aplicația folosind scurtăturile sale.
ClickFinish=Faceți clic pe Finalizează pentru a închide programul de instalare.
FinishedRestartLabel=Pentru a finaliza instalarea [name], computerul trebuie repornit. Reporniți acum?
FinishedRestartMessage=Pentru a finaliza instalarea [name], computerul trebuie repornit.%n%nReporniți acum?
ShowReadmeCheck=Da, doresc să văd fișierul Readme
YesRadio=Da, repornește computerul acum
NoRadio=Nu, voi reporni mai târziu
; used for example as 'Run MyProg.exe'
RunEntryExec=Rulează %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Vizualizează %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Introduceți discul următor
SelectDiskLabel2=Vă rugăm să introduceți discul %1 și să faceți clic pe OK.%n%nDacă fișierele de pe acest disc se găsesc într-un folder diferit de cel afișat mai jos, introduceți calea corectă sau faceți clic pe Răsfoire.
PathLabel=&Cale:
FileNotInDir2=Fișierul "%1" nu a fost găsit în "%2". Introduceți discul corect sau selectați un alt folder.
SelectDirectoryLabel=Vă rugăm să specificați calea către următorul disc.

; *** Installation phase messages
SetupAborted=Instalarea nu a fost finalizată cu succes.%n%nRemediați problema și rulați din nou programul de instalare.
AbortRetryIgnoreSelectAction=Selectați o acțiune
AbortRetryIgnoreRetry=Reîncearcă
AbortRetryIgnoreIgnore=Ignoră eroarea și continuă
AbortRetryIgnoreCancel=Anulează instalarea
RetryCancelSelectAction=Selectați o acțiune
RetryCancelRetry=Reîncearcă
RetryCancelCancel=Anulare

; *** Installation status messages
StatusClosingApplications=Se închid aplicațiile...
StatusCreateDirs=Se creează directoarele...
StatusExtractFiles=Se extrag fișierele...
StatusDownloadFiles=Se descarcă fișierele...
StatusCreateIcons=Se creează scurtăturile...
StatusCreateIniEntries=Se creează intrările INI...
StatusCreateRegistryEntries=Se creează intrările în registru...
StatusRegisterFiles=Se înregistrează fișierele...
StatusSavingUninstall=Se salvează informațiile pentru dezinstalare...
StatusRunProgram=Se finalizează instalarea...
StatusRestartingApplications=Se repornesc aplicațiile...
StatusRollback=Se revocă modificările...

; *** Misc. errors
ErrorInternal2=Eroare internă: %1
ErrorFunctionFailedNoCode=%1: eșuat
ErrorFunctionFailed=%1: eșuat; cod %2
ErrorFunctionFailedWithMessage=%1: eșuat; cod %2.%n%3
ErrorExecutingProgram=Nu se poate executa fișierul:%n%1

; *** Registry errors
ErrorRegOpenKey=Eroare la deschiderea cheii de registru:%n%1\%2
ErrorRegCreateKey=Eroare la crearea cheii de registru:%n%1\%2
ErrorRegWriteKey=Eroare la scrierea în cheia de registru:%n%1\%2

; *** INI errors
ErrorIniEntry=Eroare la crearea intrării în fișierul INI "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=Sari peste acest fișier (nerecomandat)
FileAbortRetryIgnoreIgnoreNotRecommended=Ignoră eroarea și continuă (nerecomandat)
SourceIsCorrupted=Fișierul sursă este corupt
SourceVerificationFailed=Verificarea fișierului sursă a eșuat: %1
VerificationSignatureDoesntExist=Fișierul de semnătură "%1" nu există
VerificationSignatureInvalid=Fișierul de semnătură "%1" nu este valid
VerificationKeyNotFound=Fișierul de semnătură "%1" utilizează o cheie necunoscută
VerificationFileNameIncorrect=Numele fișierului este incorect
VerificationFileTagIncorrect=Eticheta fișierului este incorectă
VerificationFileSizeIncorrect=Dimensiunea fișierului este incorectă
VerificationFileHashIncorrect=Hash-ul fișierului este incorect
SourceDoesntExist=Fișierul "%1" nu există
ExistingFileReadOnly2=Nu se poate înlocui fișierul existent deoarece este marcat ca fișier doar pentru citire.
ExistingFileReadOnlyRetry=Elimină atributul "doar pentru citire" și reîncearcă
ExistingFileReadOnlyKeepExisting=Păstrează fișierul existent
ErrorReadingExistingDest=A apărut o eroare la încercarea de a citi fișierul destinație existent:
FileExistsSelectAction=Selectați o acțiune
FileExists2=Fișierul există deja.
FileExistsOverwriteExisting=Suprascrie fișierul existent
FileExistsKeepExisting=Păstrează fișierul existent
FileExistsOverwriteOrKeepAll=Fă același lucru pentru conflictele următoare
ExistingFileNewerSelectAction=Selectați o acțiune
ExistingFileNewer2=Fișierul existent este mai nou decât cel instalat.
ExistingFileNewerOverwriteExisting=Suprascrie fișierul existent
ExistingFileNewerKeepExisting=Păstrează fișierul existent (recomandat)
ExistingFileNewerOverwriteOrKeepAll=Fă același lucru pentru conflictele următoare
ErrorChangingAttr=A apărut o eroare la încercarea de a modifica atributele fișierului existent:
ErrorCreatingTemp=A apărut o eroare la încercarea de a crea fișierul în folderul destinație:
ErrorReadingSource=A apărut o eroare la încercarea de a citi fișierul sursă:
ErrorCopying=A apărut o eroare la încercarea de a copia fișierul:
ErrorDownloading=A apărut o eroare la încercarea de a descărca fișierul:
ErrorExtracting=A apărut o eroare la încercarea de a extrage fișierele din arhivă:
ErrorReplacingExistingFile=A apărut o eroare la încercarea de a înlocui fișierul existent:
ErrorRestartReplace=Eroare RestartReplace:
ErrorRenamingTemp=A apărut o eroare la încercarea de a redenumi fișierul în folderul destinație:
ErrorRegisterServer=Nu se poate înregistra DLL/OCX: %1
ErrorRegSvr32Failed=Eroare la rularea RegSvr32, cod de returnare %1
ErrorRegisterTypeLib=Programul de instalare nu poate înregistra biblioteca de tipuri: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-biți
UninstallDisplayNameMark64Bit=64-biți
UninstallDisplayNameMarkAllUsers=Toți utilizatorii
UninstallDisplayNameMarkCurrentUser=Utilizator curent

; *** Post-installation errors
ErrorOpeningReadme=A apărut o eroare la încercarea de a deschide fișierul Readme.
ErrorRestartingComputer=Programul de instalare nu a putut reporni computerul. Reporniți-l manual.

; *** Uninstaller messages
UninstallNotFound=Fișierul "%1" nu există, dezinstalarea este imposibilă.
UninstallOpenError=Nu se poate deschide "%1". Dezinstalare imposibilă.
UninstallUnsupportedVer=Fișierul jurnal de dezinstalare "%1" nu este recunoscut de această versiune a programului de dezinstalare. Dezinstalare imposibilă.
UninstallUnknownEntry=S-a întâlnit o intrare necunoscută (%1) în fișierul jurnal de dezinstalare.
ConfirmUninstall=Sigur doriți să eliminați complet %1 și toate componentele sale?
UninstallOnlyOnWin64=Această aplicație poate fi dezinstalată doar pe Windows 64-biți.
OnlyAdminCanUninstall=Această aplicație poate fi dezinstalată numai de către un utilizator cu privilegii de administrator.
UninstallStatusLabel=Vă rugăm să așteptați în timp ce %1 este eliminat de pe computerul dvs.
UninstalledAll=%1 a fost eliminat complet de pe computerul dvs.
UninstalledMost=Dezinstalarea %1 este completă.%n%nNu s-au putut elimina câteva elemente. Le puteți elimina manual.
UninstalledAndNeedsRestart=Pentru a finaliza dezinstalarea %1, computerul dvs. trebuie repornit.%n%nReporniți acum?
UninstallDataCorrupted=Fișierul "%1" este corupt. Dezinstalare imposibilă.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Ștergeți fișierul partajat?
ConfirmDeleteSharedFile2=Sistemul indică faptul că următorul fișier partajat nu mai este utilizat de nicio altă aplicație. Confirmați ștergerea?%n%nDacă vreo aplicație folosește încă acest fișier și este șters, acea aplicație nu va funcționa corect. Dacă nu sunteți sigur, alegeți Nu. Fișierul rămas nu va afecta sistemul.
SharedFileNameLabel=Nume fișier:
SharedFileLocationLabel=Locație:
WizardUninstalling=Stare dezinstalare
StatusUninstalling=Se dezinstalează %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Se instalează %1.
ShutdownBlockReasonUninstallingApp=Se dezinstalează %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1, versiunea %2
AdditionalIcons=Scurtături suplimentare:
CreateDesktopIcon=Creează scurtătură pe Desktop
CreateQuickLaunchIcon=Creează scurtătură în Bara de lansare rapidă
ProgramOnTheWeb=%1 pe web
UninstallProgram=Dezinstalează %1
LaunchProgram=Lansează %1
AssocFileExtension=Asociază %1 cu extensia de fișier %2
AssocingFileExtension=Se asociază %1 cu extensia de fișier %2...
AutoStartProgramGroupDescription=Pornire automată:
AutoStartProgram=Pornește automat %1
AddonHostProgramNotFound=Nu se poate găsi %1 în locația selectată.%n%nContinuați oricum?