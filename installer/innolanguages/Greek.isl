; *** Inno Setup version 6.5.0+ Greek messages ***
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; Originally translated by Anastasis Chatzioglou, baldycom@hotmail.com
; Updated by XhmikosR [XhmikosR, my_nickname at yahoo dot com]
;

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=<0395><03BB><03BB><03B7><03BD><03B9><03BA><03AC>
LanguageID=$408
LanguageCodePage=1253
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=9
;WelcomeFontName=Segoe UI
;WelcomeFontSize=14
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15

[Messages]

; *** Application titles
SetupAppTitle=Εγκατάσταση
SetupWindowTitle=Εγκατάσταση - %1
UninstallAppTitle=Απεγκατάσταση
UninstallAppFullTitle=Απεγκατάσταση %1

; *** Misc. common
InformationTitle=Πληροφορίες
ConfirmTitle=Επιβεβαίωση
ErrorTitle=Σφάλμα

; *** SetupLdr messages
SetupLdrStartupMessage=Θα εκτελεστεί η εγκατάσταση του %1. Θέλετε να συνεχίσετε;
LdrCannotCreateTemp=Σφάλμα στη δημιουργία προσωρινού αρχείου. Η εγκατάσταση τερματίστηκε.
LdrCannotExecTemp=Αδύνατη η εκτέλεση αρχείου στον φάκελο προσωρινών αρχείων. Η εγκατάσταση τερματίστηκε.
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nΣφάλμα %2: %3
SetupFileMissing=Δεν βρίσκεται το αρχείο %1 στον κατάλογο εγκατάστασης. Παρακαλώ διορθώστε το πρόβλημα ή αποκτήστε νέο αντίγραφο του προγράμματος.
SetupFileCorrupt=Το αρχείο εγκατάστασης είναι κατεστραμμένο. Παρακαλώ αποκτήστε νέο αντίγραφο του προγράμματος.
SetupFileCorruptOrWrongVer=Τα αρχεία εγκατάστασης είναι κατεστραμμένα ή ασύμβατα με αυτή την έκδοση του προγράμματος εγκατάστασης. Παρακαλώ διορθώστε το πρόβλημα ή αποκτήστε νέο αντίγραφο του προγράμματος.
InvalidParameter=Μία άκυρη παράμετρος χρησιμοποιήθηκε στη γραμμή εντολών:%n%n%1
SetupAlreadyRunning=Η εγκατάσταση τρέχει ήδη.
WindowsVersionNotSupported=Αυτό το πρόγραμμα δεν υποστηρίζει την έκδοση των Windows που τρέχει ο υπολογιστής σας.
WindowsServicePackRequired=Αυτό το πρόγραμμα χρειάζεται %1 Service Pack %2 ή νεότερο.
NotOnThisPlatform=Αυτό το πρόγραμμα δεν μπορεί να εκτελεστεί σε %1.
OnlyOnThisPlatform=Αυτό το πρόγραμμα εκτελείται μόνο σε %1.
OnlyOnTheseArchitectures=Αυτό το πρόγραμμα μπορεί να εγκατασταθεί μονό σε Windows σχεδιασμένα για επεξεργαστές με την ακόλουθη αρχιτεκτονική:%n%n%1
WinVersionTooLowError=Αυτό το πρόγραμμα απαιτεί %1 έκδοση %2 ή νεότερη.
WinVersionTooHighError=Αυτό το πρόγραμμα δεν μπορεί να εκτελεστεί σε %1 έκδοση %2 ή νεότερη.
AdminPrivilegesRequired=Πρέπει να είστε ο Διαχειριστής συστήματος για να εγκαταστήσετε αυτό το πρόγραμμα.
PowerUserPrivilegesRequired=Πρέπει να είστε ο Διαχειριστής συστήματος ή Power User για να εγκαταστήσετε αυτό το πρόγραμμα.
SetupAppRunningError=Η εγκατάσταση εντόπισε ότι εκτελείται η εφαρμογή %1.%n%nΠαρακαλώ κλείστε την εφαρμογή τώρα και πατήστε Εντάξει για να συνεχίσετε, ή Άκυρο για έξοδο.
UninstallAppRunningError=Η απεγκατάσταση εντόπισε ότι εκτελείται η εφαρμογή %1.%n%nΠαρακαλώ κλείστε την εφαρμογή τώρα και πατήστε Εντάξει για να συνεχίσετε, ή Άκυρο για έξοδο.

; *** Startup questions
PrivilegesRequiredOverrideTitle=Καθορισμός Λειτουργίας Εγκατάστασης
PrivilegesRequiredOverrideInstruction=Καθορίστε τη λειτουργία εγκατάστασης
PrivilegesRequiredOverrideText1=Το %1 μπορεί να εγκατασταθεί για όλους τους χρήστες (απαιτεί δικαιώματα διαχειριστή), ή μόνο για εσάς.
PrivilegesRequiredOverrideText2=Το %1 μπορεί να εγκατασταθεί μόνο για εσάς, ή για όλους τους χρήστες (απαιτεί δικαιώματα διαχειριστή).
PrivilegesRequiredOverrideAllUsers=Εγκατάσταση για &όλους τους χρήστες
PrivilegesRequiredOverrideAllUsersRecommended=Εγκατάσταση για &όλους τους χρήστες (προτεινόμενο)
PrivilegesRequiredOverrideCurrentUser=Εγκατάσταση μόνο για &εμένα
PrivilegesRequiredOverrideCurrentUserRecommended=Εγκατάσταση μόνο για &εμένα (προτεινόμενο)

; *** Misc. errors
ErrorCreatingDir=Η εγκατάσταση δεν μπορεί να δημιουργήσει τον φάκελο "%1"
ErrorTooManyFilesInDir=Δεν μπορεί να δημιουργηθεί ένα αρχείο στον φάκελο "%1" επειδή ήδη περιέχει πολλά αρχεία

; *** Setup common messages
ExitSetupTitle=Έξοδος από τον Οδηγό Εγκατάστασης
ExitSetupMessage=Η εγκατάσταση δεν έχει ολοκληρωθεί. Αν την ακυρώσετε τώρα, το πρόγραμμα δεν θα εγκατασταθεί.%n%nΜπορείτε να εκτελέσετε ξανά την εγκατάσταση αργότερα.%n%nΤερματισμός εγκατάστασης;
AboutSetupMenuItem=&Σχετικά με την Εγκατάσταση...
AboutSetupTitle=Σχετικά με την Εγκατάσταση
AboutSetupMessage=%1 έκδοση %2%n%3%n%n%1 προσωπική σελίδα:%n%4
AboutSetupNote=
TranslatorNote=Αρχική μετάφραση από τον Anastasis Chatzioglou.%nΕνημερώσεις και βελτιώσεις από τον XhmikosR, my_nickname at yahoo dot com

; *** Buttons
ButtonBack=< &Πίσω
ButtonNext=&Επόμενο >
ButtonInstall=&Εγκατάσταση
ButtonOK=Ε&ντάξει
ButtonCancel=&Ακυρο
ButtonYes=Ν&αι
ButtonYesToAll=Ναι σε &Ολα
ButtonNo=Ό&χι
ButtonNoToAll=Όχι &σε όλα
ButtonFinish=&Τέλος
ButtonBrowse=&Αναζήτηση...
ButtonWizardBrowse=&Αναζήτηση...
ButtonNewFolder=&Δημιουργία νέου φακέλου

; *** "Select Language" dialog messages
SelectLanguageTitle=Επιλογή γλώσσας εγκατάστασης
SelectLanguageLabel=Επιλέξτε τη γλώσσα που θα χρησιμοποιηθεί κατά τη διάρκεια της εγκατάστασης:

; *** Common wizard text
ClickNext=Πατήστε Επόμενο για να συνεχίσετε ή Άκυρο για να τερματίσετε την εγκατάσταση.
BeveledLabel=
BrowseDialogTitle=Εύρεση φακέλου
BrowseDialogLabel=Επιλέξτε ένα φάκελο από την ακόλουθη λίστα και μετά πατήστε Εντάξει.
NewFolderName=Νέος φάκελος

; *** "Welcome" wizard page
WelcomeLabel1=Καλωσορίσατε στην εγκατάσταση του [name]
WelcomeLabel2=Θα γίνει εγκατάσταση του [name/ver] στον υπολογιστή σας.%n%nΣας συνιστούμε να κλείσετε κάθε άλλη εφαρμογή πριν συνεχίσετε.

; *** "Password" wizard page
WizardPassword=Εισαγωγή Κωδικού
PasswordLabel1=Αυτή η εγκατάσταση προστατεύεται με κωδικό.
PasswordLabel3=Παρακαλώ εισάγετε τον κωδικό και πατήστε Επόμενο. Οι κωδικοί είναι ευαίσθητοι πεζών/κεφαλαίων.
PasswordEditLabel=&Κωδικός:
IncorrectPassword=Ο κωδικός που εισάγατε είναι λάθος. Παρακαλώ, προσπαθήστε ξανά.

; *** "License Agreement" wizard page
WizardLicense=Άδεια Χρήσης
LicenseLabel=Παρακαλώ διαβάστε προσεκτικά τις παρακάτω πληροφορίες πριν συνεχίσετε.
LicenseLabel3=Παρακαλώ διαβάστε την ακόλουθη Άδεια Χρήσης. Πρέπει να αποδεχθείτε τους όρους πριν συνεχίσετε την εγκατάσταση.
LicenseAccepted=&Αποδέχομαι τους όρους της Άδειας Χρήσης
LicenseNotAccepted=Δεν &αποδέχομαι τους όρους της Άδειας Χρήσης

; *** "Information" wizard pages
WizardInfoBefore=Πληροφορίες
InfoBeforeLabel=Παρακαλώ διαβάστε τις παρακάτω σημαντικές πληροφορίες πριν συνεχίσετε.
InfoBeforeClickLabel=Όταν είστε έτοιμοι να συνεχίσετε με την εγκατάσταση, πατήστε Επόμενο.
WizardInfoAfter=Πληροφορίες
InfoAfterLabel=Παρακαλώ διαβάστε τις παρακάτω σημαντικές πληροφορίες πριν συνεχίσετε.
InfoAfterClickLabel=Όταν είστε έτοιμοι να συνεχίσετε με την εγκατάσταση, πατήστε Επόμενο.

; *** "User Information" wizard page
WizardUserInfo=Πληροφορίες Χρήστη
UserInfoDesc=Παρακαλώ εισάγετε τις πληροφορίες σας.
UserInfoName=&Ονομα Χρήστη:
UserInfoOrg=&Εταιρεία:
UserInfoSerial=&Σειριακός Αριθμός:
UserInfoNameRequired=Πρέπει να εισάγετε ένα όνομα.

; *** "Select Destination Location" wizard page
WizardSelectDir=Επιλέξτε τον φάκελο στον οποίο θα εγκατασταθεί το πρόγραμμα
SelectDirDesc=Πού θα εγκατασταθεί το [name];
SelectDirLabel3=Το [name] θα εγκατασταθεί στον ακόλουθο φάκελο.
SelectDirBrowseLabel=Για συνέχεια πατήστε Επόμενο. Αν θέλετε άλλο φάκελο, πατήστε Αναζήτηση.
DiskSpaceGBLabel=Χρειάζεστε τουλάχιστον [gb] GB χώρο στον δίσκο για να εγκαταστήσετε το πρόγραμμα.
DiskSpaceMBLabel=Χρειάζεστε τουλάχιστον [mb] MB χώρο στον δίσκο για να εγκαταστήσετε το πρόγραμμα.
CannotInstallToNetworkDrive=Η εγκατάσταση δεν μπορεί να γίνει σε δίσκο δικτύου.
CannotInstallToUNCPath=Η εγκατάσταση δεν μπορεί να γίνει σε διαδρομή UNC.
InvalidPath=Πρέπει να δώσετε την πλήρη διαδρομή με το γράμμα δίσκου: π.χ. %n%nC:\APP%n%nή μια διαδρομή UNC της μορφής:%n%n\\server\share
InvalidDrive=Ο τοπικός δίσκος ή ο δίσκος δικτύου που επιλέξατε δεν υπάρχει ή δεν είναι προσβάσιμος. Επιλέξτε άλλον.
DiskSpaceWarningTitle=Δεν υπάρχει αρκετός χώρος στο δίσκο
DiskSpaceWarning=Η εγκατάσταση χρειάζεται τουλάχιστον %1 KB ελεύθερο χώρο στο δίσκο αλλά ο επιλεγμένος οδηγός διαθέτει μόνον %2 KB.%n%nΘέλετε να συνεχίσετε οπωσδήποτε;
DirNameTooLong=Το όνομα του φακέλου είναι πολύ μεγάλο.
InvalidDirName=Λάθος όνομα φακέλου.
BadDirName32=Τα ονόματα φακέλων δεν μπορούν να περιέχουν κάποιον από τους παρακάτω χαρακτήρες:%n%n%1
DirExistsTitle=Ο φάκελος υπάρχει
DirExists=Ο φάκελος:%n%n%1%n%nυπάρχει ήδη. Θέλετε να γίνει η εγκατάσταση σε αυτόν τον φάκελο;
DirDoesntExistTitle=Ο φάκελος δεν υπάρχει
DirDoesntExist=Ο φάκελος:%n%n%1%n%nδεν υπάρχει. Θέλετε να δημιουργηθεί;

; *** "Select Components" wizard page
WizardSelectComponents=Επιλογή Συστατικών
SelectComponentsDesc=Ποια συστατικά θέλετε να εγκατασταθούν;
SelectComponentsLabel2=Επιλέξτε τα συστατικά που θέλετε να εγκαταστήσετε και απενεργοποιήστε αυτά που δεν θέλετε. Πατήστε "Επόμενο" όταν είστε έτοιμοι να συνεχίσετε.
FullInstallation=Πλήρης Εγκατάσταση
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Περιορισμένη Εγκατάσταση
CustomInstallation=Προσαρμοσμένη Εγκατάσταση
NoUninstallWarningTitle=Τα συστατικά υπάρχουν
NoUninstallWarning=Η εγκατάσταση εντόπισε ότι τα ακόλουθα συστατικά είναι ήδη εγκατεστημένα στον υπολογιστή σας:%n%n%1%n%nΑποεπιλέγοντας αυτά τα συστατικά δεν θα απεγκατασταθούν.%n%nΘέλετε να συνεχίσετε παρόλα αυτά;
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Η τρέχουσα επιλογή απαιτεί τουλάχιστον [gb] GB ελεύθερο χώρο στον δίσκο.
ComponentsDiskSpaceMBLabel=Η τρέχουσα επιλογή απαιτεί τουλάχιστον [mb] MB ελεύθερο χώρο στον δίσκο.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Επιλογή Επιπλέον Ενεργειών
SelectTasksDesc=Ποιες επιπλέον ενέργειες θέλετε να γίνουν;
SelectTasksLabel2=Επιλέξτε τις επιπλέον ενέργειες που θέλετε να γίνουν κατά την εγκατάσταση του [name] και πατήστε Επόμενο.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Επιλογή Καταλόγου Στο Μενού Εκκίνηση
SelectStartMenuFolderDesc=Πού θα τοποθετηθούν οι συντομεύσεις του προγράμματος;
SelectStartMenuFolderLabel3=Η εγκατάσταση θα δημιουργήσει τις συντομεύσεις του προγράμματος στον ακόλουθο κατάλογο του μενού Έναρξης.
SelectStartMenuFolderBrowseLabel=Για συνέχεια, πατήστε Επόμενο. Αν θέλετε άλλο κατάλογο, πατήστε Αναζήτηση.
MustEnterGroupName=Πρέπει να δώσετε το όνομα ενός καταλόγου.
GroupNameTooLong=Το όνομα του καταλόγου είναι πολύ μεγάλο.
InvalidGroupName=Το όνομα του καταλόγου δεν είναι σωστό.
BadGroupName=Ονόματα καταλόγων δεν μπορούν να περιέχουν κάποιον από τους παρακάτω χαρακτήρες:%n%n%1
NoProgramGroupCheck2=&Χωρίς δημιουργία καταλόγου στο μενού Έναρξης

; *** "Ready to Install" wizard page
WizardReady=Έτοιμος για εγκατάσταση
ReadyLabel1=Η εγκατάσταση του [name] είναι έτοιμη να εκτελεστεί στον υπολογιστή σας.
ReadyLabel2a=Πατήστε "Εγκατάσταση" για να συνεχίσετε ή "Πίσω" αν θέλετε να αλλάξετε κάποιες ρυθμίσεις.
ReadyLabel2b=Πατήστε "Εγκατάσταση" για να συνεχίσετε την εγκατάσταση.
ReadyMemoUserInfo=Πληροφορίες Χρήστη:
ReadyMemoDir=Φάκελος προορισμού:
ReadyMemoType=Είδος εγκατάστασης:
ReadyMemoComponents=Επιλεγμένα συστατικά:
ReadyMemoGroup=Κατάλογος στο μενού Έναρξη:
ReadyMemoTasks=Επιπλέον Ενέργειες:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Λήψη πρόσθετων αρχείων...
ButtonStopDownload=Δι&ακοπή λήψης
StopDownload=Είστε σίγουροι ότι θέλετε να διακόψετε τη λήψη;
ErrorDownloadAborted=Η λήψη ακυρώθηκε
ErrorDownloadFailed=Αποτυχία λήψης: %1 %2
ErrorDownloadSizeFailed=Σφάλμα ανάγνωσης μεγέθους: %1 %2
ErrorProgress=Μη έγκυρη πρόοδος: %1 από %2
ErrorFileSize=Μη έγκυρο μέγεθος αρχείου: αναμενόμενο %1, βρέθηκε %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractingLabel=Αποσυμπίεση πρόσθετων αρχείων...
ButtonStopExtraction=Δι&ακοπή αποσυμπίεσης
StopExtraction=Είστε σίγουροι ότι θέλετε να διακόψετε την αποσυμπίεση;
ErrorExtractionAborted=Η αποσυμπίεση διακόπηκε
ErrorExtractionFailed=Αποτυχία αποσυμπίεσης: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=Λάθος κωδικός πρόσβασης
ArchiveIsCorrupted=Το αρχείο είναι κατεστραμμένο
ArchiveUnsupportedFormat=Μη υποστηριζόμενη μορφή αρχείου

; *** "Preparing to Install" wizard page
WizardPreparing=Προετοιμασία Εγκατάστασης
PreparingDesc=Η εγκατάσταση προετοιμάζεται να εγκαταστήσει το πρόγραμμα [name] στον υπολογιστή σας.
PreviousInstallNotCompleted=Η εγκατάσταση/αφαίρεση ενός προηγούμενου προγράμματος δεν ολοκληρώθηκε. Θα χρειαστεί να επανεκκινήσετε τον υπολογιστή σας ώστε να ολοκληρώσετε εκείνη την εγκατάσταση.%n%nΜετά την επανεκκίνηση του υπολογιστή σας, εκτελέστε την Εγκατάσταση πάλι για να ολοκληρώσετε την εγκατάσταση του [name].
CannotContinue=Η εγκατάσταση δε μπορεί να συνεχίσει. Παρακαλώ πατήστε "Ακύρωση" για τερματισμό.
ApplicationsFound=Οι ακόλουθες εφαρμογές χρησιμοποιούν αρχεία τα οποία πρέπει να ενημερωθούν από την Εγκατάσταση. Προτείνεται να επιτρέψετε στην Εγκατάσταση να κλείσει αυτές τις εφαρμογές αυτόματα.
ApplicationsFound2=Οι ακόλουθες εφαρμογές χρησιμοποιούν αρχεία τα οποία πρέπει να ενημερωθούν από την Εγκατάσταση. Προτείνεται να επιτρέψετε στην Εγκατάσταση να κλείσει αυτές τις εφαρμογές αυτόματα. Μετά την ολοκλήρωση της διαδικασίας, η Εγκατάσταση θα προσπαθήσει να επανεκκινήσει τις εφαρμογές.
CloseApplications=&Αυτόματο κλείσιμο των εφαρμογών
DontCloseApplications=&Χωρίς κλείσιμο των εφαρμογών
ErrorCloseApplications=Η Εγκατάσταση απέτυχε να κλείσει αυτόματα όλες τις εφαρμογές. Προτείνεται να κλείσετε όλες τις εφαρμογές που χρησιμοποιούν αρχεία τα οποία πρέπει να ενημερωθούν από την Εγκατάσταση προτού συνεχίσετε.
PrepareToInstallNeedsRestart=Η εγκατάσταση πρέπει να επανεκκινήσει τον υπολογιστή σας. Μετά την επανεκκίνηση, εκτελέστε ξανά την εγκατάσταση για να ολοκληρώσετε την εγκατάσταση του [name].%n%nΘέλετε να γίνει επανεκκίνηση τώρα;

; *** "Installing" wizard page
WizardInstalling=Πρόοδος Εγκατάστασης
InstallingLabel=Παρακαλώ περιμένετε να ολοκληρωθεί η εγκατάσταση του [name] στον υπολογιστή σας.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Ολοκληρώνοντας τον Οδηγό Εγκατάστασης του [name]
FinishedLabelNoIcons=Η εγκατάσταση του [name] στον υπολογιστή σας ολοκληρώθηκε με επιτυχία.
FinishedLabel=Η εγκατάσταση του [name] στον υπολογιστή σας ολοκληρώθηκε με επιτυχία. Μπορείτε να ξεκινήσετε το πρόγραμμα επιλέγοντας τα εικονίδια που δημιουργήθηκαν.
ClickFinish=Πατήστε Τέλος για να τερματίσετε το πρόγραμμα εγκατάστασης.
FinishedRestartLabel=Για να ολοκληρωθεί η εγκατάσταση του [name] πρέπει να γίνει επανεκκίνηση του υπολογιστή σας. Θέλετε να γίνει επανεκκίνηση τώρα;
FinishedRestartMessage=Για να ολοκληρωθεί η εγκατάσταση του [name] πρέπει να γίνει επανεκκίνηση του υπολογιστή σας.%n%nΘέλετε να γίνει επανεκκίνηση τώρα;
ShowReadmeCheck=Ναι, θέλω να δω το αρχείο README
YesRadio=&Ναι, να γίνει επανεκκίνηση τώρα
NoRadio=&Όχι, θα κάνω επανεκκίνηση αργότερα
; used for example as 'Run MyProg.exe'
RunEntryExec=Εκτέλεση %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Προβολή %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Η Εγκατάσταση χρειάζεται τον επόμενο δίσκο/δισκέτα
SelectDiskLabel2=Παρακαλώ, εισάγετε το Δίσκο/Δισκέτα %1 και πατήστε Εντάξει.%n%nΑν τα αρχεία αυτού του δίσκου/δισκέτας βρίσκονται σε κάποιον φάκελο εκτός αυτού που φαίνεται παρακάτω, εισάγετε τη σωστή διαδρομή ή πατήστε Αναζήτηση.
PathLabel=&Διαδρομή:
FileNotInDir2=Το αρχείο "%1" δε βρέθηκε στο "%2". Παρακαλώ εισάγετε το σωστό δίσκο/δισκέτα ή επιλέξτε κάποιον άλλο φάκελο.
SelectDirectoryLabel=Παρακαλώ καθορίσετε την τοποθεσία του επόμενου δίσκου/δισκέτας.

; *** Installation phase messages
SetupAborted=Η εγκατάσταση δεν ολοκληρώθηκε.%n%nΠαρακαλώ, διορθώστε το πρόβλημα και εκτελέστε ξανά την Εγκατάσταση.
AbortRetryIgnoreSelectAction=Επιλέξτε ενέργεια
AbortRetryIgnoreRetry=Δο&κιμάστε ξανά
AbortRetryIgnoreIgnore=&Αγνοήστε το σφάλμα και συνεχίστε
AbortRetryIgnoreCancel=Ακύρωση εγκατάστασης
RetryCancelSelectAction=Επιλέξτε ενέργεια
RetryCancelRetry=&Δοκιμάστε ξανά
RetryCancelCancel=Ακύρωση

; *** Installation status messages
StatusClosingApplications=Κλείσιμο εφαρμογών...
StatusCreateDirs=Δημιουργία φακέλων...
StatusExtractFiles=Αποσυμπίεση αρχείων...
StatusDownloadFiles=Λήψη αρχείων...
StatusCreateIcons=Δημιουργία εικονιδίων...
StatusCreateIniEntries=Καταχώρηση στο αρχείο INI...
StatusCreateRegistryEntries=Καταχώρηση στο μητρώο συστήματος...
StatusRegisterFiles=Καταχώρηση αρχείων...
StatusSavingUninstall=Αποθήκευση πληροφοριών απεγκατάστασης...
StatusRunProgram=Τελειώνοντας την εγκατάσταση...
StatusRestartingApplications=Επανεκκίνηση εφαρμογών...
StatusRollback=Επαναφορά αλλαγών...

; *** Misc. errors
ErrorInternal2=Εσωτερικό σφάλμα: %1
ErrorFunctionFailedNoCode=Αποτυχία %1
ErrorFunctionFailed=Αποτυχία %1; κωδικός σφάλματος %2
ErrorFunctionFailedWithMessage=Αποτυχία %1; κωδικός σφάλματος %2.%n%3
ErrorExecutingProgram=Αδύνατη η εκτέλεση του αρχείου:%n%1

; *** Registry errors
ErrorRegOpenKey=Δεν μπορεί να διαβαστεί το κλειδί μητρώου συστήματος:%n%1\%2
ErrorRegCreateKey=Δεν μπορεί να δημιουργηθεί το κλειδί μητρώου συστήματος:%n%1\%2
ErrorRegWriteKey=Δεν μπορεί να γίνει καταχώρηση στο κλειδί μητρώου συστήματος:%n%1\%2

; *** INI errors
ErrorIniEntry=Δεν μπορεί να γίνει καταχώρηση στο αρχείο INI "%1".

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=&Παράβλεψη αυτού του αρχείου (δεν προτείνεται)
FileAbortRetryIgnoreIgnoreNotRecommended=&Αγνόηση σφάλματος και συνέχεια (δεν προτείνεται)
SourceIsCorrupted=Το αρχείο προέλευσης είναι κατεστραμμένο
SourceVerificationFailed=Αποτυχία επαλήθευσης αρχείου προέλευσης: %1
VerificationSignatureDoesntExist=Το αρχείο υπογραφής "%1" δεν υπάρχει
VerificationSignatureInvalid=Το αρχείο υπογραφής "%1" δεν είναι έγκυρο
VerificationKeyNotFound=Το αρχείο υπογραφής "%1" χρησιμοποιεί άγνωστο κλειδί
VerificationFileNameIncorrect=Λάθος όνομα αρχείου
VerificationFileTagIncorrect=Λάθος ετικέτα αρχείου
VerificationFileSizeIncorrect=Λάθος μέγεθος αρχείου
VerificationFileHashIncorrect=Λάθος hash αρχείου
SourceDoesntExist=Το αρχείο προέλευσης "%1" δεν υπάρχει
ExistingFileReadOnly2=Δεν είναι δυνατή η αντικατάσταση του υπάρχοντος αρχείου επειδή είναι μόνο για ανάγνωση.
ExistingFileReadOnlyRetry=&Αφαίρεση μόνο για ανάγνωση και δοκιμή ξανά
ExistingFileReadOnlyKeepExisting=&Διατήρηση υπάρχοντος αρχείου
ErrorReadingExistingDest=Παρουσιάστηκε σφάλμα κατά την ανάγνωση του υπάρχοντος αρχείου:
FileExistsSelectAction=Επιλέξτε ενέργεια
FileExists2=Το αρχείο υπάρχει ήδη.
FileExistsOverwriteExisting=&Αντικατάσταση υπάρχοντος αρχείου
FileExistsKeepExisting=&Διατήρηση υπάρχοντος αρχείου
FileExistsOverwriteOrKeepAll=&Εκτέλεση αυτής της ενέργειας για μελλοντικές συγκρούσεις
ExistingFileNewerSelectAction=Επιλέξτε ενέργεια
ExistingFileNewer2=Το υπάρχον αρχείο είναι νεότερο από αυτό που θα εγκαταστήσει η εγκατάσταση.
ExistingFileNewerOverwriteExisting=&&Αντικατάσταση υπάρχοντος αρχείου
ExistingFileNewerKeepExisting=&Διατήρηση υπάρχοντος αρχείου (προτεινόμενο)
ExistingFileNewerOverwriteOrKeepAll=&Εκτέλεση αυτής της ενέργειας για μελλοντικές συγκρούσεις
ErrorChangingAttr=Προέκυψε σφάλμα στην προσπάθεια να αλλαχτούν τα χαρακτηριστικά του υπάρχοντος αρχείου:
ErrorCreatingTemp=Προέκυψε σφάλμα στην προσπάθεια να δημιουργηθεί ένα αρχείο στον κατάλογο προορισμού:
ErrorReadingSource=Προέκυψε σφάλμα στην προσπάθεια ανάγνωσης του αρχείου προέλευσης:
ErrorCopying=Προέκυψε σφάλμα στην προσπάθεια να αντιγραφεί το αρχείο:
ErrorDownloading=Προέκυψε σφάλμα κατά την προσπάθεια λήψης αρχείου:
ErrorExtracting=Προέκυψε σφάλμα κατά την προσπάθεια αποσυμπίεσης αρχείου:
ErrorReplacingExistingFile=Προέκυψε σφάλμα στην προσπάθεια να αντικατασταθεί το υπάρχον αρχείο:
ErrorRestartReplace=Αποτυχία επανεκκίνησης "Αντικατάσταση":
ErrorRenamingTemp=Προέκυψε σφάλμα στην προσπάθεια μετονομασίας ενός αρχείου στον κατάλογο προορισμού:
ErrorRegisterServer=Αδυναμία εγγραφής αρχείων DLL/OCX: %1
ErrorRegSvr32Failed=Αποτυχία RegSvr32 με κωδικό εξόδου %1
ErrorRegisterTypeLib=Αδύνατη η καταχώρηση της βιβλιοθήκης τύπων: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Όλοι οι χρήστες
UninstallDisplayNameMarkCurrentUser=Τρέχων χρήστης

; *** Post-installation errors
ErrorOpeningReadme=Προέκυψε σφάλμα στην προσπάθεια να ανοίξει το αρχείο README.
ErrorRestartingComputer=Προέκυψε σφάλμα στην προσπάθεια επανεκκίνησης του υπολογιστή. Παρακαλώ επανεκκινήσετε τον υπολογιστή σας μόνοι σας.

; *** Uninstaller messages
UninstallNotFound=Το αρχείο "%1" δεν βρέθηκε. Η απεγκατάσταση δεν μπορεί να γίνει.
UninstallOpenError=Το αρχείο "%1" δεν μπόρεσε να φορτωθεί. Η απεγκατάσταση δεν μπορεί να γίνει
UninstallUnsupportedVer=Το αρχείο καταγραφής απεγκατάστασης "%1" είναι σε μη αναγνωρίσιμη μορφή από αυτή την έκδοση του προγράμματος απεγκατάστασης. Η απεγκατάσταση δεν μπορεί να εκτελεστεί
UninstallUnknownEntry=Άγνωστη καταχώρηση (%1) βρέθηκε στο αρχείο καταγραφής απεγκατάστασης
ConfirmUninstall=Είστε σίγουροι ότι θέλετε να διαγράψετε το %1 και όλα τα συστατικά του;
UninstallOnlyOnWin64=Αυτή η εφαρμογή μπορεί να απεγκατασταθεί μόνο σε 64-bit Windows.
OnlyAdminCanUninstall=Η απεγκατάσταση μπορεί να εκτελεστεί μόνο από τον Διαχειριστή συστήματος.
UninstallStatusLabel=Παρακαλώ περιμένετε όσο το %1 διαγράφεται από τον υπολογιστή σας.
UninstalledAll=Η απεγκατάσταση του %1 έγινε με επιτυχία.
UninstalledMost=Η απεγκατάσταση του %1 έγινε με επιτυχία.%n%nΚάποια συστατικά δεν ήταν δυνατόν να διαγραφούν. Αυτά μπορούν να διαγραφούν από εσάς.
UninstalledAndNeedsRestart=Για να ολοκληρώσετε την απεγκατάσταση του %1, θα πρέπει να επανεκκινήσετε τον υπολογιστή σας.%n%nΘα θέλατε να κάνετε επανεκκίνηση τώρα;
UninstallDataCorrupted=Το αρχείο "%1" είναι κατεστραμμένο. Η απεγκατάσταση δεν μπορεί να γίνει

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Διαγραφή κοινόχρηστου αρχείου;
ConfirmDeleteSharedFile2=Το σύστημα δείχνει ότι το ακόλουθο κοινόχρηστο αρχείο δεν χρησιμοποιείται πλέον από κανένα πρόγραμμα. Θέλετε η απεγκατάσταση να διαγράψει αυτό το κοινόχρηστο αρχείο;%n%nΑν κάποιο πρόγραμμα χρησιμοποιεί ακόμα αυτό το αρχείο και διαγραφεί, το πρόγραμμα μπορεί να μη λειτουργεί σωστά. Αν δεν είστε σίγουροι, επιλέξτε Όχι. Αφήνοντας το αρχείο στο σύστημά σας δεν προκαλεί βλάβη.
SharedFileNameLabel=Όνομα Αρχείου:
SharedFileLocationLabel=Τοποθεσία:
WizardUninstalling=Κατάσταση Απεγκατάστασης
StatusUninstalling=Απεγκατάσταση του %1...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=Εγκατάσταση του %1.
ShutdownBlockReasonUninstallingApp=Απεγκατάσταση του %1.

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 έκδοση %2
AdditionalIcons=Επιπλέον εικονίδια:
CreateDesktopIcon=Δημιουργία εικονιδίου στην επιφάνεια εργασίας
CreateQuickLaunchIcon=Δημιουργία εικονιδίου στη Γρήγορη Εκκίνηση
ProgramOnTheWeb=Το %1 στο Internet
UninstallProgram=Απεγκατάσταση του %1
LaunchProgram=Εκκίνηση του %1
AssocFileExtension=Σύνδεση %1 με την επέκταση αρχείου %2
AssocingFileExtension=Σύνδεση %1 με την επέκταση αρχείου %2...
AutoStartProgramGroupDescription=Έναρξη:
AutoStartProgram=Αυτόματη εκκίνηση του %1
AddonHostProgramNotFound=Το %1 δε βρέθηκε στο φάκελο που επιλέξατε.%n%nΘέλετε να συνεχίσετε παρόλα αυτά;