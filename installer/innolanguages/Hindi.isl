; *** Inno Setup version 6.5.0+ Hindi messages ***
; Translated by Him Prasad Gautam [ drishtibachak at gmail.com ]
; To download user-contributed translations of this file, go to:
; https://jrsoftware.org/files/istrans/
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).

[LangOptions]
; The following three entries are very important. Be sure to read and 
; understand the '[LangOptions] section' topic in the help file.
LanguageName=हिंदी
LanguageID=$0439
LanguageCodePage=0
RightToLeft=no
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=10
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Segoe UI
;WelcomeFontSize=12
;TitleFontName=
;TitleFontSize=35
;CopyrightFontName=
;CopyrightFontSize=9

[Messages]

; *** Application titles
SetupAppTitle=स्थापना
SetupWindowTitle=स्थापना - %1
UninstallAppTitle=निस्कासन
UninstallAppFullTitle=%1 कि निस्कासन

; *** Misc. common
InformationTitle=सूचना
ConfirmTitle=पुष्टिकरण
ErrorTitle=त्रुटि

; *** SetupLdr messages
SetupLdrStartupMessage=इस सेटअप %1 को आपके कंप्यूटर में स्थापित करेगा। क्या आप आगे बढ़ना चाहते हैं?
LdrCannotCreateTemp=अस्थायी फ़ाइलें नहीं बना सका, सेटअप विफल रहा।
LdrCannotExecTemp=अस्थायी फ़ोल्डर से फ़ाइलें निष्पादित नहीं कर सका। सेटअप विफल रहा।
HelpTextNote=

; *** Startup error messages
LastErrorMessage=%1.%n%nत्रुटि %2: %3
SetupFileMissing=फ़ाइल %1 स्थापना निर्देशिका में नहीं है। कृपया समस्या ठीक करें या प्रोग्राम की नई प्रति प्राप्त करें।
SetupFileCorrupt=सेटअप फ़ाइलें खराब हैं। कृपया प्रोग्राम की नई प्रति प्राप्त करें।
SetupFileCorruptOrWrongVer=सेटअप फ़ाइलें खराब हैं या सेटअप के इस संस्करण के साथ असंगत हैं। कृपया समस्या ठीक करें या प्रोग्राम की नई प्रति प्राप्त करें।
InvalidParameter=कमांड लाइन में अमान्य तर्क पारित किए गए: %n%n%1
SetupAlreadyRunning=सेटअप पहले से ही चल रहा है।
WindowsVersionNotSupported=यह प्रोग्राम आपके कंप्यूटर पर चल रहे Windows संस्करण का समर्थन नहीं करता।
WindowsServicePackRequired=इस प्रोग्राम के लिए %1 सर्विस पैक %2 या अधिक आवश्यक है।
NotOnThisPlatform=यह प्रोग्राम %1 पर नहीं चलेगा।
OnlyOnThisPlatform=यह प्रोग्राम केवल %1 पर ही चलना चाहिए।
OnlyOnTheseArchitectures=इस प्रोग्राम को केवल निम्न प्रोसेसर आर्किटेक्चर के लिए डिज़ाइन किए गए Windows संस्करणों पर स्थापित किया जा सकता है:%n%n%1
WinVersionTooLowError=इस प्रोग्राम के लिए %1 संस्करण %2 या अधिक आवश्यक है।
WinVersionTooHighError=इस प्रोग्राम को %1 संस्करण %2 या अधिक पर स्थापित नहीं किया जा सकता।
AdminPrivilegesRequired=इस प्रोग्राम को स्थापित करने के लिए आपको व्यवस्थापक के रूप में लॉग इन होना चाहिए।
PowerUserPrivilegesRequired=इस प्रोग्राम को स्थापित करने के लिए आपको व्यवस्थापक के रूप में लॉग इन होना चाहिए या Power Users समूह का सदस्य होना चाहिए।
SetupAppRunningError=सेटअप ने पता लगाया है कि %1 पहले से चल रहा है।%n%nकृपया अभी सभी उदाहरण बंद करें, फिर जारी रखने के लिए ठीक या बाहर निकलने के लिए रद्द करें क्लिक करें।
UninstallAppRunningError=अनइंस्टॉलर ने पता लगाया है कि %1 पहले से चल रहा है।%n%nकृपया अभी सभी उदाहरण बंद करें, फिर जारी रखने के लिए ठीक या बाहर निकलने के लिए रद्द करें क्लिक करें।

; *** Startup questions
PrivilegesRequiredOverrideTitle=स्थापना मोड चुनें
PrivilegesRequiredOverrideInstruction=स्थापना मोड चुनें
PrivilegesRequiredOverrideText1=%1 को सभी उपयोगकर्ताओं के लिए स्थापित किया जा सकता है (व्यवस्थापक विशेषाधिकार आवश्यक), या केवल आपके खाते के लिए।
PrivilegesRequiredOverrideText2=आप %1 को केवल अपने खाते के लिए स्थापित कर सकते हैं, या सभी उपयोगकर्ताओं के लिए (व्यवस्थापक विशेषाधिकार आवश्यक)।
PrivilegesRequiredOverrideAllUsers=सभी उपयोगकर्ताओं के लिए स्थापित करें
PrivilegesRequiredOverrideAllUsersRecommended=सभी उपयोगकर्ताओं के लिए स्थापित करें (अनुशंसित)
PrivilegesRequiredOverrideCurrentUser=केवल मेरे खाते में स्थापित करें
PrivilegesRequiredOverrideCurrentUserRecommended=केवल मेरे खाते में स्थापित करें (अनुशंसित)

; *** Misc. errors
ErrorCreatingDir=सेटअप निर्देशिका "%1" नहीं बना सका
ErrorTooManyFilesInDir=निर्देशिका "%1" में बहुत अधिक फ़ाइलें होने के कारण फ़ाइल नहीं बना सका

; *** Setup common messages
ExitSetupTitle=सेटअप विज़ार्ड से बाहर निकलें
ExitSetupMessage=स्थापना पूरी नहीं हुई। यदि आप अभी बाहर निकलते हैं, तो प्रोग्राम स्थापित नहीं होगा।%n%nआप सेटअप को बाद में फिर से चला सकते हैं ताकि स्थापना पूरी हो सके।%n%nसेटअप समाप्त करें?
AboutSetupMenuItem=सेटअप के बारे में...
AboutSetupTitle=सेटअप के बारे में
AboutSetupMessage=%1 संस्करण %2%n%3%n%n%1 वेब पेज:%n%4
AboutSetupNote=
TranslatorNote=यह हिन्दी में अनुवाद हिम प्रसाद गौतम द्वारा किया गया है।

; *** Buttons
ButtonBack=< &पिछला
ButtonNext=अ&गला >
ButtonInstall=&स्थापित करें
ButtonOK=&ठीक
ButtonCancel=रद्द &करें
ButtonYes=&हाँ
ButtonYesToAll=सभी के लिए &हाँ
ButtonNo=&नहीं
ButtonNoToAll=सभी के लिए न&हीं
ButtonFinish=&समाप्त
ButtonBrowse=ब्राउज़...
ButtonWizardBrowse=ब्राउज़...
ButtonNewFolder=नया फ़ोल्डर बनाएँ

; *** "Select Language" dialog messages
SelectLanguageTitle=सेटअप विज़ार्ड भाषा चुनें
SelectLanguageLabel=वह भाषा चुनें जिसका उपयोग स्थापना के दौरान किया जाना चाहिए।

; *** Common wizard text
ClickNext=जारी रखने के लिए अगला क्लिक करें, या स्थापना से बाहर निकलने के लिए रद्द करें क्लिक करें।
BeveledLabel=सौजन्य: हिम प्रसाद गौतम
BrowseDialogTitle=फ़ोल्डर चुनने के लिए ब्राउज़ करें
BrowseDialogLabel=नीचे सूची से एक फ़ोल्डर चुनें, फिर ठीक क्लिक करें।
NewFolderName=नया फ़ोल्डर

; *** "Welcome" wizard page
WelcomeLabel1=[name] स्थापना विज़ार्ड में आपका स्वागत है
WelcomeLabel2=यह विज़ार्ड आपके कंप्यूटर पर [name/ver] स्थापित करेगा।%n%nजारी रखने से पहले अन्य सभी एप्लिकेशन बंद करना उचित है।

; *** "Password" wizard page
WizardPassword=पासवर्ड
PasswordLabel1=यह स्थापना पासवर्ड से सुरक्षित है।
PasswordLabel3=जारी रखने के लिए कृपया पासवर्ड दें, फिर अगला क्लिक करें। पासवर्ड केस-संवेदी हैं।
PasswordEditLabel=&पासवर्ड:
IncorrectPassword=आपके द्वारा दिया गया पासवर्ड गलत है। कृपया पुनः प्रयास करें।

; *** "License Agreement" wizard page
WizardLicense=लाइसेंस समझौता
LicenseLabel=जारी रखने से पहले कृपया निम्नलिखित महत्वपूर्ण जानकारी पढ़ें।
LicenseLabel3=कृपया निम्नलिखित लाइसेंस समझौता पढ़ें। स्थापना जारी रखने से पहले इस समझौते की शर्तों को स्वीकार करना होगा।
LicenseAccepted=मैंने इस समझौते की शर्तें पढ़ ली हैं और &स्वीकार करता हूँ
LicenseNotAccepted=मैं इस समझौते की शर्तें &स्वीकार नहीं करता

; *** "Information" wizard pages
WizardInfoBefore=सूचना
InfoBeforeLabel=जारी रखने से पहले कृपया निम्नलिखित महत्वपूर्ण जानकारी पढ़ें।
InfoBeforeClickLabel=जब आप स्थापना जारी रखने के लिए तैयार हों, तो अगला क्लिक करें।
WizardInfoAfter=सूचना
InfoAfterLabel=जारी रखने से पहले कृपया निम्नलिखित महत्वपूर्ण जानकारी पढ़ें।
InfoAfterClickLabel=जब आप स्थापना जारी रखने के लिए तैयार हों, तो अगला क्लिक करें।

; *** "User Information" wizard page
WizardUserInfo=उपयोगकर्ता जानकारी
UserInfoDesc=कृपया अपनी जानकारी दर्ज करें।
UserInfoName=उपयो&क्ता नाम:
UserInfoOrg=&संगठन:
UserInfoSerial=&क्रम संख्या:
UserInfoNameRequired=नाम दर्ज किया जाना चाहिए।

; *** "Select Destination Location" wizard page
WizardSelectDir=गंतव्य स्थान चुनें
SelectDirDesc=[name] कहाँ स्थापित किया जाए?
SelectDirLabel3=स्थापना विज़ार्ड [name] को निम्न फ़ोल्डर में स्थापित करेगा।
SelectDirBrowseLabel=जारी रखने के लिए, अगला क्लिक करें। यदि आप कोई अन्य फ़ोल्डर निर्दिष्ट करना चाहते हैं, तो ब्राउज़ क्लिक करें।
DiskSpaceGBLabel=प्रोग्राम स्थापित करने के लिए कम से कम [gb] GB डिस्क स्थान की आवश्यकता है।
DiskSpaceMBLabel=प्रोग्राम स्थापित करने के लिए कम से कम [mb] MB डिस्क स्थान की आवश्यकता है।
CannotInstallToNetworkDrive=सेटअप नेटवर्क ड्राइव पर स्थापित नहीं कर सकता।
CannotInstallToUNCPath=सेटअप UNC पथ पर स्थापित नहीं कर सकता।
InvalidPath=आपको ड्राइव अक्षर सहित एक पूर्ण पथ दर्ज करना होगा; उदाहरण:%n%nC:\APP%n%या UNC पथ इस रूप में:%n%n\\server\share
InvalidDrive=आपके द्वारा चुना गया ड्राइव या UNC शेयर मौजूद नहीं है या अप्राप्य है। कृपया कोई अन्य चुनें।
DiskSpaceWarningTitle=अपर्याप्त डिस्क स्थान
DiskSpaceWarning=स्थापना के लिए कम से कम %1 KB खाली स्थान की आवश्यकता है, लेकिन चयनित ड्राइव पर केवल %2 KB उपलब्ध है।%n%nक्या आप फिर भी जारी रखना चाहते हैं?
DirNameTooLong=फ़ोल्डर नाम या पथ बहुत लंबा है।
InvalidDirName=फ़ोल्डर नाम अमान्य है।
BadDirName32=फ़ोल्डर नामों में निम्न में से कोई भी वर्ण शामिल नहीं हो सकता:%n%n%1
DirExistsTitle=फ़ोल्डर पहले से मौजूद है
DirExists=फ़ोल्डर:%n%n%1%n%nपहले से मौजूद है। क्या आप फिर भी इस फ़ोल्डर में स्थापित करना चाहते हैं?
DirDoesntExistTitle=फ़ोल्डर मौजूद नहीं है
DirDoesntExist=फ़ोल्डर:%n%n%1%n%nमौजूद नहीं है। क्या आप फ़ोल्डर बनाना चाहते हैं?

; *** "Select Components" wizard page
WizardSelectComponents=घटक चुनें
SelectComponentsDesc=कौन से घटक स्थापित किए जाने चाहिए?
SelectComponentsLabel2=उन घटकों का चयन करें जिन्हें आप स्थापित करना चाहते हैं, और उन घटकों का चयन रद्द करें जिन्हें आप स्थापित नहीं करना चाहते। जब आप जारी रखने के लिए तैयार हों, तो अगला क्लिक करें।
FullInstallation=पूर्ण स्थापना
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=संक्षिप्त स्थापना
CustomInstallation=कस्टम स्थापना
NoUninstallWarningTitle=घटक मौजूद हैं
NoUninstallWarning=सेटअप ने पता लगाया है कि निम्नलिखित घटक आपके कंप्यूटर पर पहले से स्थापित हैं:%n%n%1%n%nइन घटकों का चयन रद्द करने से वे अनइंस्टॉल नहीं होंगे।%n%nक्या आप फिर भी जारी रखना चाहते हैं?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=वर्तमान चयन के लिए कम से कम [gb] GB डिस्क स्थान की आवश्यकता है।
ComponentsDiskSpaceMBLabel=वर्तमान चयन के लिए कम से कम [mb] MB डिस्क स्थान की आवश्यकता है।

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=अतिरिक्त कार्य चुनें
SelectTasksDesc=कौन से अतिरिक्त कार्य निष्पादित किए जाने चाहिए?
SelectTasksLabel2=वे अतिरिक्त कार्य चुनें जिन्हें आप चाहते हैं कि सेटअप [name] स्थापित करते समय निष्पादित करे, फिर अगला क्लिक करें。

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=प्रारंभ मेनू फ़ोल्डर चुनें
SelectStartMenuFolderDesc=सेटअप को प्रोग्राम शॉर्टकट कहाँ रखने चाहिए?
SelectStartMenuFolderLabel3=सेटअप प्रोग्राम शॉर्टकट निम्न प्रारंभ मेनू फ़ोल्डर में बनाएगा।
SelectStartMenuFolderBrowseLabel=जारी रखने के लिए, अगला क्लिक करें। यदि आप कोई अन्य फ़ोल्डर निर्दिष्ट करना चाहते हैं, तो ब्राउज़ क्लिक करें।
MustEnterGroupName=फ़ोल्डर नाम दर्ज किया जाना चाहिए।
GroupNameTooLong=फ़ोल्डर नाम या पथ बहुत लंबा है।
InvalidGroupName=फ़ोल्डर नाम अमान्य है।
BadGroupName=फ़ोल्डर नाम में निम्न में से कोई भी वर्ण शामिल नहीं हो सकता:%n%n%1
NoProgramGroupCheck2=प्रारंभ मेनू में कोई फ़ोल्डर &न बनाएँ

; *** "Ready to Install" wizard page
WizardReady=स्थापित करने के लिए तैयार
ReadyLabel1=सेटअप अब आपके कंप्यूटर पर [name] स्थापित करने के लिए तैयार है।
ReadyLabel2a=स्थापना जारी रखने के लिए "स्थापित करें" क्लिक करें, या यदि आप किसी भी सेटिंग को समीक्षा या बदलना चाहते हैं तो "पिछला" क्लिक करें।
ReadyLabel2b=स्थापना जारी रखने के लिए स्थापित करें क्लिक करें।
ReadyMemoUserInfo=उपयोगकर्ता जानकारी:
ReadyMemoDir=गंतव्य पथ:
ReadyMemoType=स्थापना प्रकार:
ReadyMemoComponents=चयनित घटक:
ReadyMemoGroup=प्रारंभ मेनू फ़ोल्डर:
ReadyMemoTasks=अतिरिक्त कार्य:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=अतिरिक्त फ़ाइलें डाउनलोड हो रही हैं...
ButtonStopDownload=डाउनलोड रोकें
StopDownload=क्या आप वाकई डाउनलोड रोकना चाहते हैं?
ErrorDownloadAborted=डाउनलोड रद्द किया गया
ErrorDownloadFailed=डाउनलोड विफल: %1 %2
ErrorDownloadSizeFailed=आकार पढ़ने में त्रुटि: %1 %2
ErrorProgress=अमान्य प्रगति: %1 का %2
ErrorFileSize=अमान्य फ़ाइल आकार: अपेक्षित %1, प्राप्त %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractingLabel=अतिरिक्त फ़ाइलें निकाली जा रही हैं...
ButtonStopExtraction=निष्कर्षण रोकें
StopExtraction=क्या आप वाकई निष्कर्षण रोकना चाहते हैं?
ErrorExtractionAborted=निष्कर्षण रद्द किया गया
ErrorExtractionFailed=निष्कर्षण विफल: %1

; *** Archive extraction failure details
ArchiveIncorrectPassword=गलत पासवर्ड
ArchiveIsCorrupted=आर्काइव खराब है
ArchiveUnsupportedFormat=आर्काइव प्रारूप समर्थित नहीं है

; *** "Preparing to Install" wizard page
WizardPreparing=स्थापना की तैयारी कर रहा है
PreparingDesc=सेटअप आपके कंप्यूटर पर [name] स्थापित करने की तैयारी कर रहा है।
PreviousInstallNotCompleted=पिछला प्रोग्राम स्थापना/अनइंस्टॉलेशन पूरा नहीं हुआ। इस स्थापना को पूरा करने के लिए आपको कंप्यूटर को पुनरारंभ करना होगा।%n%nअपने कंप्यूटर के पुनरारंभ होने के बाद, [name] स्थापना पूरी करने के लिए सेटअप को फिर से चलाएँ।
CannotContinue=सेटअप जारी नहीं रख सकता। कृपया बाहर निकलने के लिए "रद्द करें" क्लिक करें।
ApplicationsFound=निम्नलिखित एप्लिकेशन उन फ़ाइलों का उपयोग कर रहे हैं जिन्हें सेटअप द्वारा अपडेट किए जाने की आवश्यकता है। अनुशंसा है कि आप सेटअप को इन एप्लिकेशन को स्वचालित रूप से बंद करने दें।
ApplicationsFound2=निम्नलिखित एप्लिकेशन उन फ़ाइलों का उपयोग कर रहे हैं जिन्हें सेटअप द्वारा अपडेट किए जाने की आवश्यकता है। अनुशंसा है कि आप सेटअप को इन एप्लिकेशन को स्वचालित रूप से बंद करने दें। स्थापना पूरी होने के बाद, सेटअप एप्लिकेशन को पुनरारंभ करने का प्रयास करेगा।
CloseApplications=स्वचालित रूप से एप्लिकेशन बंद करें
DontCloseApplications=एप्लिकेशन बंद न करें
ErrorCloseApplications=सेटअप सभी एप्लिकेशन स्वचालित रूप से बंद नहीं कर सका। अनुशंसा है कि जारी रखने से पहले सभी एप्लिकेशन बंद कर दें जो उन फ़ाइलों का उपयोग करते हैं जिन्हें सेटअप द्वारा अपडेट किए जाने की आवश्यकता है।
PrepareToInstallNeedsRestart=सेटअप को कंप्यूटर पुनरारंभ करना होगा। अपने कंप्यूटर के पुनरारंभ होने के बाद, [name] स्थापना पूरी करने के लिए सेटअप को फिर से चलाएँ।%n%nक्या आप अभी पुनरारंभ करना चाहते हैं?

; *** "Installing" wizard page
WizardInstalling=स्थापित किया जा रहा है
InstallingLabel=कृपया प्रतीक्षा करें जब तक सेटअप आपके कंप्यूटर पर [name] स्थापित करता है।

; *** "Setup Completed" wizard page
FinishedHeadingLabel=[name] स्थापना विज़ार्ड पूरा हो रहा है
FinishedLabelNoIcons=सेटअप ने [name] को आपके कंप्यूटर पर सफलतापूर्वक स्थापित कर दिया है।
FinishedLabel=सेटअप ने [name] को आपके कंप्यूटर पर स्थापित कर दिया है। प्रोग्राम स्थापित शॉर्टकट का चयन करके चलाया जा सकता है।
ClickFinish=सेटअप विज़ार्ड से बाहर निकलने के लिए समाप्त क्लिक करें
FinishedRestartLabel=[name] स्थापना पूरी करने के लिए, सेटअप को आपके कंप्यूटर को पुनरारंभ करना होगा। क्या आप अभी पुनरारंभ करना चाहते हैं?
FinishedRestartMessage=[name] स्थापना पूरी करने के लिए, सेटअप को आपके कंप्यूटर को पुनरारंभ करना होगा।%n%nक्या आप अभी पुनरारंभ करना चाहते हैं?
ShowReadmeCheck=हाँ, मैं Readme फ़ाइल देखना चाहता हूँ
YesRadio=हाँ, अब कंप्यूटर पुनरारंभ करें
NoRadio=नहीं, मैं बाद में कंप्यूटर पुनरारंभ करूँगा
; used for example as 'Run MyProg.exe'
RunEntryExec=चलाएँ %1
; used for example as 'View Readme.txt'
RunEntryShellExec=देखें %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=सेटअप को अगली डिस्क चाहिए
SelectDiskLabel2=कृपया डिस्क %1 डालें और ठीक क्लिक करें।%n%nयदि इस डिस्क की फ़ाइलें नीचे दिखाए गए से भिन्न फ़ोल्डर में पाई जा सकती हैं, तो सही पथ दर्ज करें या ब्राउज़ क्लिक करें।
PathLabel=&पथ:
FileNotInDir2=फ़ाइल "%1" को "%2" में नहीं पाया जा सका। कृपया सही डिस्क डालें या कोई अन्य फ़ोल्डर चुनें।
SelectDirectoryLabel=कृपया अगली डिस्क का स्थान निर्दिष्ट करें।

; *** Installation phase messages
SetupAborted=स्थापना पूरी नहीं हुई।%n%nकृपया समस्या ठीक करें और सेटअप फिर से चलाएँ।
AbortRetryIgnoreSelectAction=कार्रवाई चुनें
AbortRetryIgnoreRetry=पुनः प्रयास करें
AbortRetryIgnoreIgnore=त्रुटि को नज़रअंदाज करें और जारी रखें
AbortRetryIgnoreCancel=स्थापना रद्द करें
RetryCancelSelectAction=कार्रवाई चुनें
RetryCancelRetry=पुनः प्रयास करें
RetryCancelCancel=रद्द करें

; *** Installation status messages
StatusClosingApplications=एप्लिकेशन बंद किए जा रहे हैं...
StatusCreateDirs=निर्देशिकाएँ बनाई जा रही हैं...
StatusExtractFiles=फ़ाइलें निकाली जा रही हैं...
StatusDownloadFiles=फ़ाइलें डाउनलोड हो रही हैं...
StatusCreateIcons=शॉर्टकट बनाए जा रहे हैं...
StatusCreateIniEntries=INI प्रविष्टियाँ बनाई जा रही हैं...
StatusCreateRegistryEntries=रजिस्ट्री प्रविष्टियाँ बनाई जा रही हैं...
StatusRegisterFiles=फ़ाइलें पंजीकृत की जा रही हैं...
StatusSavingUninstall=अनइंस्टॉलेशन जानकारी सहेजी जा रही है...
StatusRunProgram=स्थापना समाप्त की जा रही है...
StatusRestartingApplications=एप्लिकेशन पुनरारंभ किए जा रहे हैं...
StatusRollback=परिवर्तन वापस किए जा रहे हैं...

; *** Misc. errors
ErrorInternal2=आंतरिक त्रुटि: %1
ErrorFunctionFailedNoCode=%1 विफल
ErrorFunctionFailed=%1 विफल; कोड %2
ErrorFunctionFailedWithMessage=%1 विफल; कोड %2।%n%3
ErrorExecutingProgram=फ़ाइल निष्पादित नहीं कर सका:%n%1

; *** Registry errors
ErrorRegOpenKey=रजिस्ट्री कुंजी खोलने में त्रुटि:%n%1\%2
ErrorRegCreateKey=रजिस्ट्री कुंजी बनाने में त्रुटि:%n%1\%2
ErrorRegWriteKey=रजिस्ट्री कुंजी में लिखने में त्रुटि:%n%1\%2

; *** INI errors
ErrorIniEntry=फ़ाइल "%1" में INI प्रविष्टि बनाने में त्रुटि हुई।

; *** File copying errors
FileAbortRetryIgnoreSkipNotRecommended=इस फ़ाइल को छोड़ें (अनुशंसित नहीं)
FileAbortRetryIgnoreIgnoreNotRecommended=त्रुटि को नज़रअंदाज करें और जारी रखें (अनुशंसित नहीं)
SourceIsCorrupted=स्रोत फ़ाइल खराब है
SourceVerificationFailed=स्रोत फ़ाइल सत्यापन विफल: %1
VerificationSignatureDoesntExist=हस्ताक्षर फ़ाइल "%1" मौजूद नहीं है
VerificationSignatureInvalid=हस्ताक्षर फ़ाइल "%1" अमान्य है
VerificationKeyNotFound=हस्ताक्षर फ़ाइल "%1" एक अज्ञात कुंजी का उपयोग करती है
VerificationFileNameIncorrect=फ़ाइल नाम गलत है
VerificationFileTagIncorrect=फ़ाइल टैग गलत है
VerificationFileSizeIncorrect=फ़ाइल आकार गलत है
VerificationFileHashIncorrect=फ़ाइल हैश गलत है
SourceDoesntExist=फ़ाइल "%1" मौजूद नहीं है
ExistingFileReadOnly2=मौजूदा फ़ाइल को प्रतिस्थापित नहीं किया जा सकता क्योंकि इसे केवल पढ़ने के लिए चिह्नित किया गया है।
ExistingFileReadOnlyRetry=फ़ाइलों से केवल-पढ़ने का गुण हटाएँ और पुनः प्रयास करें
ExistingFileReadOnlyKeepExisting=मौजूदा फ़ाइल रखें
ErrorReadingExistingDest=मौजूदा फ़ाइल पढ़ने का प्रयास करते समय एक त्रुटि हुई:
FileExistsSelectAction=कार्रवाई चुनें
FileExists2=फ़ाइल पहले से मौजूद है।
FileExistsOverwriteExisting=मौजूदा फ़ाइल को अधिलेखित करें
FileExistsKeepExisting=मौजूदा फ़ाइल रखें
FileExistsOverwriteOrKeepAll=आगामी संघर्षों के लिए ऐसा ही करें
ExistingFileNewerSelectAction=कार्रवाई चुनें
ExistingFileNewer2=मौजूदा फ़ाइल उस फ़ाइल से नई है जिसे सेटअप स्थापित करेगा।
ExistingFileNewerOverwriteExisting=मौजूदा फ़ाइल को अधिलेखित करें
ExistingFileNewerKeepExisting=मौजूदा फ़ाइल रखें (अनुशंसित)
ExistingFileNewerOverwriteOrKeepAll=आगामी संघर्षों के लिए ऐसा ही करें
ErrorChangingAttr=मौजूदा फ़ाइल के विशेषताएँ बदलने का प्रयास करते समय एक त्रुटि हुई:
ErrorCreatingTemp=गंतव्य निर्देशिका में फ़ाइल बनाने का प्रयास करते समय एक त्रुटि हुई:
ErrorReadingSource=स्रोत फ़ाइल पढ़ने का प्रयास करते समय एक त्रुटि हुई:
ErrorCopying=फ़ाइल की प्रतिलिपि बनाने का प्रयास करते समय एक त्रुटि हुई:
ErrorDownloading=फ़ाइल डाउनलोड करने का प्रयास करते समय एक त्रुटि हुई:
ErrorExtracting=आर्काइव फ़ाइलें निकालने का प्रयास करते समय एक त्रुटि हुई:
ErrorReplacingExistingFile=मौजूदा फ़ाइल को प्रतिस्थापित करने का प्रयास करते समय एक त्रुटि हुई:
ErrorRestartReplace="प्रतिस्थापन" पुनरारंभ विफल:
ErrorRenamingTemp=गंतव्य निर्देशिका में फ़ाइल का नाम बदलने का प्रयास करते समय एक त्रुटि हुई:
ErrorRegisterServer=DLL/OCX फ़ाइल पंजीकृत नहीं कर सका: %1
ErrorRegSvr32Failed=RegSvr32 निकास कोड %1 के साथ विफल रहा
ErrorRegisterTypeLib=टाइप लाइब्रेरी पंजीकृत नहीं कर सका: %1

; *** Uninstall display name markings
; used for example as 'My Program (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; used for example as 'My Program (32-bit, All users)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-बिट
UninstallDisplayNameMark64Bit=64-बिट
UninstallDisplayNameMarkAllUsers=सभी उपयोगकर्ता
UninstallDisplayNameMarkCurrentUser=वर्तमान उपयोगकर्ता

; *** Post-installation errors
ErrorOpeningReadme=Readme फ़ाइल खोलने का प्रयास करते समय एक त्रुटि हुई।
ErrorRestartingComputer=सेटअप कंप्यूटर पुनरारंभ नहीं कर सका। कृपया मैन्युअल रूप से ऐसा करें।

; *** Uninstaller messages
UninstallNotFound=फ़ाइल "%1" मौजूद नहीं है। अनइंस्टॉलेशन संभव नहीं है।
UninstallOpenError="%1" खोल नहीं सका। अनइंस्टॉलेशन संभव नहीं है।
UninstallUnsupportedVer=अनइंस्टॉलेशन लॉग फ़ाइल "%1" इस अनइंस्टॉलर संस्करण द्वारा अज्ञात प्रारूप में है। अनइंस्टॉलेशन संभव नहीं है।
UninstallUnknownEntry=अनइंस्टॉलेशन लॉग में एक अज्ञात प्रविष्टि (%1) मिली।
ConfirmUninstall=क्या आप वाकई %1 को, और इसके सभी घटकों को, पूरी तरह से हटाना चाहते हैं?
UninstallOnlyOnWin64=इस स्थापना को केवल 64-बिट Windows पर ही अनइंस्टॉल किया जा सकता है।
OnlyAdminCanUninstall=इस प्रोग्राम को केवल व्यवस्थापक विशेषाधिकार वाले उपयोगकर्ता ही अनइंस्टॉल कर सकते हैं।
UninstallStatusLabel=कृपया प्रतीक्षा करें जब तक %1 को आपके कंप्यूटर से हटाया जा रहा है।
UninstalledAll=%1 को आपके कंप्यूटर से सफलतापूर्वक हटा दिया गया है।
UninstalledMost=%1 हटा दिया गया है।%n%nकुछ आइटम हटाए नहीं जा सके। उन्हें मैन्युअल रूप से हटाया जा सकता है।
UninstalledAndNeedsRestart=%1 अनइंस्टॉलेशन पूरा करने के लिए, कंप्यूटर को पुनरारंभ करना होगा।%n%nक्या आप अभी पुनरारंभ करना चाहते हैं?
UninstallDataCorrupted=फ़ाइल "%1" खराब है। अनइंस्टॉलेशन संभव नहीं है।

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=साझा फ़ाइल हटाएँ?
ConfirmDeleteSharedFile2=सिस्टम इंगित करता है कि निम्नलिखित साझा फ़ाइल अब किसी भी प्रोग्राम द्वारा उपयोग में नहीं है। क्या आप चाहते हैं कि सेटअप इस साझा फ़ाइल को हटा दे?%n%nयदि कोई प्रोग्राम अभी भी इस फ़ाइल का उपयोग कर रहा है और इसे हटा दिया जाता है, तो वे प्रोग्राम ठीक से काम नहीं कर सकते। यदि आप अनिश्चित हैं, तो नहीं चुनें। इस फ़ाइल को आपके सिस्टम पर छोड़ने से कोई नुकसान नहीं होगा।
SharedFileNameLabel=फ़ाइल नाम:
SharedFileLocationLabel=स्थान:
WizardUninstalling=अनइंस्टॉलेशन स्थिति
StatusUninstalling=%1 अनइंस्टॉल किया जा रहा है...

; *** Shutdown block reasons
ShutdownBlockReasonInstallingApp=%1 स्थापित किया जा रहा है।
ShutdownBlockReasonUninstallingApp=%1 अनइंस्टॉल किया जा रहा है।

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 संस्करण %2
AdditionalIcons=अतिरिक्त शॉर्टकट:
CreateDesktopIcon=डेस्कटॉप पर शॉर्टकट बनाएँ
CreateQuickLaunchIcon=त्वरित प्रारंभ शॉर्टकट बनाएँ
ProgramOnTheWeb=%1 वेब पर
UninstallProgram=%1 अनइंस्टॉल करें
LaunchProgram=%1 चलाएँ
AssocFileExtension=%2 फ़ाइल एक्सटेंशन के साथ %1 संबद्ध करें
AssocingFileExtension=%2 फ़ाइल एक्सटेंशन के साथ %1 संबद्ध किया जा रहा है...
AutoStartProgramGroupDescription=स्वचालित प्रारंभ:
AutoStartProgram=%1 स्वचालित रूप से चलाएँ
AddonHostProgramNotFound=चुने गए स्थान पर %1 नहीं मिला।%n%nक्या आप फिर भी जारी रखना चाहते हैं?