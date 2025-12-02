; *** Inno Setup version 6.5.0+ Indonesian messages ***
;
; Untuk mengunduh berkas terjemahan hasil konstribusi pengguna, kunjungi: 
;   https://jrsoftware.org/files/istrans/
;
; Alih bahasa oleh: MozaikTM (mozaik.tm@gmail.com)
;
; Catatan: Saat menerjemahkan pesan ini, jangan masukkan titik (.) pada
; akhir pesan tanpa titik, karena Inno Setup menambahkan titik pada pesan tersebut
; secara otomatis (menambahkan sebuah titik akan memunculkan dua titik).

[LangOptions]
; Tiga baris berikut sangat penting. Pastikan untuk membaca dan 
; memahami topik 'bagian [LangOption]' dalam berkas bantuan.
LanguageName=Bahasa Indonesia
LanguageID=$0421
LanguageCodePage=0
RightToLeft=no
; Bila target bahasa Anda memerlukan fon atau ukuran khusus,
; hapus tanda komentar (;) dari salah satu atau beberapa baris berikut dan ubah seperlunya.
;DialogFontName=
;DialogFontSize=8
;DialogFontBaseScaleWidth=7
;DialogFontBaseScaleHeight=15
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Judul aplikasi
SetupAppTitle=Pemasang
SetupWindowTitle=Pemasangan - %1
UninstallAppTitle=Pelepas
UninstallAppFullTitle=Pelepasan %1

; *** Misc. common
InformationTitle=Informasi
ConfirmTitle=Konfirmasi
ErrorTitle=Kesalahan

; *** Pesan untuk SetupLdr
SetupLdrStartupMessage=Pemasang ini akan memasang %1. Lanjutkan?
LdrCannotCreateTemp=Tidak dapat membuat berkas sementara. Pemasangan gagal
LdrCannotExecTemp=Tidak dapat mengeksekusi berkas dari direktori sementara. Pemasangan gagal
HelpTextNote=

; *** Pesan kesalahan saat memuat Pemasang
LastErrorMessage=%1.%n%nKesalahan %2: %3
SetupFileMissing=Berkas %1 hilang dari direktori instalasi. Silakan perbaiki masalah atau dapatkan salinan baru program ini.
SetupFileCorrupt=Berkas setup rusak. Silakan dapatkan salinan baru program ini.
SetupFileCorruptOrWrongVer=Berkas setup rusak atau tidak kompatibel dengan versi setup ini. Silakan perbaiki masalah atau dapatkan salinan baru program ini.
InvalidParameter=Parameter tidak valid diteruskan ke baris perintah:%n%n%1
SetupAlreadyRunning=Setup sudah berjalan.
WindowsVersionNotSupported=Program ini tidak mendukung versi Windows yang berjalan di komputer Anda.
WindowsServicePackRequired=Program ini memerlukan %1 Service Pack %2 atau yang lebih baru.
NotOnThisPlatform=Program ini tidak akan berjalan pada %1.
OnlyOnThisPlatform=Program ini harus dijalankan pada %1.
OnlyOnTheseArchitectures=Program ini hanya dapat dipasang pada versi Windows yang dirancang untuk arsitektur prosesor berikut:%n%n%1
WinVersionTooLowError=Program ini memerlukan %1 versi %2 atau yang lebih baru.
WinVersionTooHighError=Program ini tidak dapat dipasang pada %1 versi %2 atau yang lebih baru.
AdminPrivilegesRequired=Anda harus masuk sebagai administrator saat memasang program ini.
PowerUserPrivilegesRequired=Anda harus masuk sebagai administrator atau anggota grup Power Users saat memasang program ini.
SetupAppRunningError=Pemasang mendeteksi bahwa %1 sedang berjalan.%n%nSilakan tutup semua instans sekarang, lalu klik OK untuk melanjutkan atau Batal untuk keluar.
UninstallAppRunningError=Pelepas mendeteksi bahwa %1 sedang berjalan.%n%nSilakan tutup semua instans sekarang, lalu klik OK untuk melanjutkan atau Batal untuk keluar.

; *** Pertanyaan saat memuat Pemasang
PrivilegesRequiredOverrideTitle=Pilih Mode Instalasi
PrivilegesRequiredOverrideInstruction=Pilih mode instalasi
PrivilegesRequiredOverrideText1=%1 dapat dipasang untuk semua pengguna (memerlukan hak istimewa administrator), atau hanya untuk akun Anda.
PrivilegesRequiredOverrideText2=Anda dapat memasang %1 hanya untuk akun Anda, atau untuk semua pengguna (memerlukan hak istimewa administrator).
PrivilegesRequiredOverrideAllUsers=Pasang untuk &semua pengguna
PrivilegesRequiredOverrideAllUsersRecommended=Pasang untuk &semua pengguna (disarankan)
PrivilegesRequiredOverrideCurrentUser=Pasang hanya untuk &akun saya
PrivilegesRequiredOverrideCurrentUserRecommended=Pasang hanya untuk &akun saya (disarankan)

; *** Macam-macam galat
ErrorCreatingDir=Pemasang tidak dapat membuat direktori "%1"
ErrorTooManyFilesInDir=Tidak dapat membuat berkas di direktori "%1" karena berisi terlalu banyak berkas.

; *** Pesan umum pada Pemasang
ExitSetupTitle=Keluar dari Pemasang
ExitSetupMessage=Pemasangan tidak lengkap. Jika Anda keluar sekarang, program tidak akan terpasang.%n%nAnda dapat menjalankan pemasang lagi nanti untuk menyelesaikan instalasi.%n%nKeluar dari pemasang?
AboutSetupMenuItem=&Tentang Pemasang...
AboutSetupTitle=Tentang Pemasang
AboutSetupMessage=%1 versi %2%n%3%n%n%1 halaman web:%n%4
AboutSetupNote=
TranslatorNote=Bila Anda menemukan typo (kesalahan pengetikan), terjemahan yang salah atau kurang tepat, atau Anda ingin mendapatkan terjemahan untuk versi lawas, silakan kirimkan surel (email) ke mozaik(dot)tm(at)gmail(dot)com

; *** Tombol-tombol
ButtonBack=< &Sebelumnya
ButtonNext=&Berikutnya >
ButtonInstall=&Pasang
ButtonOK=&Oke
ButtonCancel=&Batal
ButtonYes=&Ya
ButtonYesToAll=Ya untuk &semua
ButtonNo=&Tidak
ButtonNoToAll=Tidak untuk se&mua
ButtonFinish=&Selesai
ButtonBrowse=&Jelajahi...
ButtonWizardBrowse=&Jelajahi...
ButtonNewFolder=&Buat Folder Baru

; *** Halaman "Pilih Bahasa"
SelectLanguageTitle=Pilih Bahasa Pemasang
SelectLanguageLabel=Pilih bahasa untuk digunakan selama instalasi.

; *** Pesan umum pada Pemasang
ClickNext=Klik Berikutnya untuk melanjutkan, atau Batal untuk keluar dari pemasang.
BeveledLabel=
BrowseDialogTitle=Jelajahi untuk Memilih Folder
BrowseDialogLabel=Pilih folder dari daftar di bawah, lalu klik Oke.
NewFolderName=Folder Baru

; *** Halaman "Selamat Datang"
WelcomeLabel1=Selamat datang di Pemasang [name]
WelcomeLabel2=Pemasang ini akan memasang [name/ver] di komputer Anda.%n%nDisarankan untuk menutup semua aplikasi lain sebelum melanjutkan.

; *** Halaman "Kata Sandi"
WizardPassword=Kata Sandi
PasswordLabel1=Instalasi ini dilindungi kata sandi.
PasswordLabel3=Silakan berikan kata sandi, lalu klik Berikutnya untuk melanjutkan. Kata sandi bersifat case-sensitive.
PasswordEditLabel=&Kata Sandi:
IncorrectPassword=Kata sandi yang Anda masukkan salah. Silakan coba lagi.

; *** Halaman "Kesepakatan Lisensi"
WizardLicense=Perjanjian Lisensi
LicenseLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
LicenseLabel3=Silakan baca perjanjian lisensi berikut. Anda harus menyetujui syarat perjanjian ini sebelum melanjutkan instalasi.
LicenseAccepted=Saya telah membaca dan &menyetujui syarat perjanjian ini
LicenseNotAccepted=Saya &tidak menyetujui perjanjian ini

; *** Halaman "Informasi"
WizardInfoBefore=Informasi
InfoBeforeLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
InfoBeforeClickLabel=Ketika Anda siap untuk melanjutkan instalasi, klik Berikutnya.
WizardInfoAfter=Informasi
InfoAfterLabel=Silakan baca informasi penting berikut sebelum melanjutkan.
InfoAfterClickLabel=Ketika Anda siap untuk melanjutkan instalasi, klik Berikutnya.

; *** Halaman "Informasi Pengguna"
WizardUserInfo=Informasi Pengguna
UserInfoDesc=Silakan masukkan informasi Anda.
UserInfoName=&Nama Pengguna:
UserInfoOrg=&Organisasi:
UserInfoSerial=&Nomor Seri:
UserInfoNameRequired=Nama harus dimasukkan.

; *** Halaman "Pilih Lokasi Tujuan"
WizardSelectDir=Pilih Lokasi Tujuan
SelectDirDesc=Di mana [name] harus dipasang?
SelectDirLabel3=Pemasang akan memasang [name] di folder berikut.
SelectDirBrowseLabel=Untuk melanjutkan, klik Berikutnya. Jika Anda ingin memilih folder lain, klik Jelajahi.
DiskSpaceGBLabel=Diperlukan setidaknya [gb] GB ruang kosong untuk memasang program.
DiskSpaceMBLabel=Diperlukan setidaknya [mb] MB ruang kosong untuk memasang program.
CannotInstallToNetworkDrive=Pemasang tidak dapat menginstal ke drive jaringan.
CannotInstallToUNCPath=Pemasang tidak dapat menginstal ke jalur UNC.
InvalidPath=Anda harus memasukkan jalur lengkap dengan huruf drive; contoh:%n%nC:\APP%n%atau jalur UNC dalam format:%n%n\\server\share
InvalidDrive=Drive atau berbagi UNC yang Anda pilih tidak ada atau tidak dapat diakses. Silakan pilih yang lain.
DiskSpaceWarningTitle=Ruang Disk Tidak Cukup
DiskSpaceWarning=Pemasang memerlukan setidaknya %1 KB ruang kosong untuk menginstal, tetapi drive yang dipilih hanya memiliki %2 KB tersedia.%n%nLanjutkan tetap?
DirNameTooLong=Nama folder atau jalur terlalu panjang.
InvalidDirName=Nama folder tidak valid.
BadDirName32=Nama folder tidak boleh berisi karakter berikut:%n%n%1
DirExistsTitle=Folder Sudah Ada
DirExists=Folder:%n%n%1%n%nsudah ada. Apakah Anda ingin menginstal ke folder ini tetap?
DirDoesntExistTitle=Folder Tidak Ada
DirDoesntExist=Folder:%n%n%1%n%ntidak ada. Apakah Anda ingin membuat folder?

; *** Halaman "Pilih Komponen"
WizardSelectComponents=Pilih Komponen
SelectComponentsDesc=Komponen mana yang harus diinstal?
SelectComponentsLabel2=Pilih komponen yang ingin Anda instal, dan hapus centang pada komponen yang tidak ingin Anda instal. Klik Berikutnya ketika Anda siap untuk melanjutkan.
FullInstallation=Instalasi Penuh
; jika memungkinkan jangan terjemahkan 'Compact' sebagai 'Minimal' (maksudnya 'Minimal' dalam bahasa Anda)
CompactInstallation=Instalasi Ringkas
CustomInstallation=Instalasi Kustom
NoUninstallWarningTitle=Komponen Sudah Ada
NoUninstallWarning=Pemasang mendeteksi bahwa komponen berikut sudah terinstal di komputer Anda:%n%n%1%n%nMenghapus centang komponen ini tidak akan menghapus instalasinya.%n%nLanjutkan tetap?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceGBLabel=Pilihan saat ini memerlukan setidaknya [gb] GB ruang disk.
ComponentsDiskSpaceMBLabel=Pilihan saat ini memerlukan setidaknya [mb] MB ruang disk.

; *** Halaman "Pilih Tugas Tambahan"
WizardSelectTasks=Pilih Tugas Tambahan
SelectTasksDesc=Tugas tambahan mana yang harus dilakukan?
SelectTasksLabel2=Pilih tugas tambahan yang Anda ingin pemasang lakukan saat menginstal [name], lalu klik Berikutnya.

; *** Halaman "Pilih Folder Start Menu"
WizardSelectProgramGroup=Pilih Folder Start Menu
SelectStartMenuFolderDesc=Di mana pemasang harus meletakkan pintasan program?
SelectStartMenuFolderLabel3=Pemasang akan membuat pintasan program di folder Start Menu berikut.
SelectStartMenuFolderBrowseLabel=Untuk melanjutkan, klik Berikutnya. Jika Anda ingin memilih folder lain, klik Jelajahi.
MustEnterGroupName=Nama folder harus dimasukkan.
GroupNameTooLong=Nama folder atau jalur terlalu panjang.
InvalidGroupName=Nama folder tidak valid.
BadGroupName=Nama folder tidak boleh berisi karakter berikut:%n%n%1
NoProgramGroupCheck2=&Jangan buat folder Start Menu

; *** Halaman "Siap Memasang"
WizardReady=Siap Memasang
ReadyLabel1=Pemasang sekarang siap untuk mulai menginstal [name] di komputer Anda.
ReadyLabel2a=Klik "Pasang" untuk melanjutkan dengan instalasi, atau "Sebelumnya" jika Anda ingin meninjau atau mengubah setelan apa pun.
ReadyLabel2b=Klik Pasang untuk melanjutkan instalasi.
ReadyMemoUserInfo=Informasi Pengguna:
ReadyMemoDir=Jalur Tujuan:
ReadyMemoType=Jenis Instalasi:
ReadyMemoComponents=Komponen yang Dipilih:
ReadyMemoGroup=Folder Start Menu:
ReadyMemoTasks=Tugas Tambahan:

; *** TDownloadWizardPage wizard page and DownloadTemporaryFile
DownloadingLabel2=Mengunduh berkas tambahan...
ButtonStopDownload=H&entikan Pengunduhan
StopDownload=Apakah Anda yakin ingin menghentikan pengunduhan?
ErrorDownloadAborted=Pengunduhan dibatalkan
ErrorDownloadFailed=Pengunduhan gagal: %1 %2
ErrorDownloadSizeFailed=Kesalahan membaca ukuran: %1 %2
ErrorProgress=Kemajuan tidak valid: %1 dari %2
ErrorFileSize=Ukuran berkas tidak valid: diharapkan %1, didapat %2

; *** TExtractionWizardPage wizard page and Extract7ZipArchive
ExtractingLabel=Mengekstrak berkas tambahan...
ButtonStopExtraction=H&entikan Ekstraksi
StopExtraction=Apakah Anda yakin ingin menghentikan ekstraksi?
ErrorExtractionAborted=Ekstraksi dibatalkan
ErrorExtractionFailed=Ekstraksi gagal: %1

; *** Detail kegagalan ekstraksi arsip
ArchiveIncorrectPassword=Kata sandi salah
ArchiveIsCorrupted=Arsip rusak
ArchiveUnsupportedFormat=Format arsip tidak didukung

; *** Halaman "Bersiap Memasang"
WizardPreparing=Mempersiapkan Instalasi
PreparingDesc=Pemasang sedang mempersiapkan untuk menginstal [name] di komputer Anda.
PreviousInstallNotCompleted=Instalasi/uninstal program sebelumnya tidak lengkap. Anda perlu me-restart komputer untuk menyelesaikan instalasi ini.%n%nSetelah komputer Anda restart, jalankan pemasang lagi untuk menyelesaikan instalasi [name].
CannotContinue=Pemasang tidak dapat melanjutkan. Silakan klik "Batal" untuk keluar.
ApplicationsFound=Aplikasi berikut menggunakan berkas yang perlu diperbarui oleh pemasang. Disarankan agar Anda mengizinkan pemasang untuk menutup aplikasi ini secara otomatis.
ApplicationsFound2=Aplikasi berikut menggunakan berkas yang perlu diperbarui oleh pemasang. Disarankan agar Anda mengizinkan pemasang untuk menutup aplikasi ini secara otomatis. Setelah instalasi selesai, pemasang akan mencoba memulai ulang aplikasi.
CloseApplications=Tutup aplikasi secara &otomatis
DontCloseApplications=&Jangan tutup aplikasi
ErrorCloseApplications=Pemasang tidak dapat menutup semua aplikasi secara otomatis. Disarankan agar Anda menutup semua aplikasi yang menggunakan berkas yang perlu diperbarui oleh pemasang sebelum melanjutkan.
PrepareToInstallNeedsRestart=Pemasang harus me-restart komputer. Setelah komputer Anda restart, jalankan pemasang lagi untuk menyelesaikan instalasi [name].%n%nRestart sekarang?

; *** Halaman "Memasang"
WizardInstalling=Menginstal
InstallingLabel=Harap tunggu sementara pemasang menginstal [name] di komputer Anda.

; *** Halaman "Pemasangan Selesai"
FinishedHeadingLabel=Menyelesaikan Pemasang [name]
FinishedLabelNoIcons=Pemasang telah selesai menginstal [name] di komputer Anda.
FinishedLabel=Pemasang telah selesai menginstal [name] di komputer Anda. Program dapat dijalankan dengan memilih pintasan yang terinstal.
ClickFinish=Klik Selesai untuk keluar dari pemasang.
FinishedRestartLabel=Untuk menyelesaikan instalasi [name], pemasang harus me-restart komputer Anda. Restart sekarang?
FinishedRestartMessage=Untuk menyelesaikan instalasi [name], pemasang harus me-restart komputer Anda.%n%nRestart sekarang?
ShowReadmeCheck=Ya, saya ingin melihat berkas Readme
YesRadio=&Ya, restart komputer sekarang
NoRadio=&Tidak, saya akan restart komputer nanti
; digunakan misalnya untuk 'Jalankan MyProg.exe'
RunEntryExec=Jalankan %1
; digunakan misalnya untuk 'Lihat Readme.txt'
RunEntryShellExec=Lihat %1

; *** Hal-hal terkait "Setup Needs the Next Disk"
ChangeDiskTitle=Pemasang Membutuhkan Disk Berikutnya
SelectDiskLabel2=Harap masukkan disk %1 dan klik Oke.%n%nJika berkas pada disk ini dapat ditemukan di folder selain yang ditampilkan di bawah, masukkan jalur yang benar atau klik Jelajahi.
PathLabel=&Jalur:
FileNotInDir2=Tidak dapat menemukan berkas "%1" di "%2". Harap masukkan disk yang benar atau pilih folder lain.
SelectDirectoryLabel=Harap tentukan lokasi disk berikutnya.

; *** Pesan fase instalasi
SetupAborted=Instalasi tidak lengkap.%n%nHarap perbaiki masalah dan jalankan pemasang lagi.
AbortRetryIgnoreSelectAction=Pilih tindakan
AbortRetryIgnoreRetry=&Coba lagi
AbortRetryIgnoreIgnore=&Abaikan kesalahan dan lanjutkan
AbortRetryIgnoreCancel=Batalkan instalasi
RetryCancelSelectAction=Pilih tindakan
RetryCancelRetry=&Coba lagi
RetryCancelCancel=Batal

; *** Pesan status instalasi
StatusClosingApplications=Menutup aplikasi...
StatusCreateDirs=Membuat direktori...
StatusExtractFiles=Mengekstrak berkas...
StatusDownloadFiles=Mengunduh berkas...
StatusCreateIcons=Membuat pintasan...
StatusCreateIniEntries=Membuat entri INI...
StatusCreateRegistryEntries=Membuat entri Registry...
StatusRegisterFiles=Mendaftarkan berkas...
StatusSavingUninstall=Menyimpan informasi uninstall...
StatusRunProgram=Menyelesaikan instalasi...
StatusRestartingApplications=Memulai ulang aplikasi...
StatusRollback=Mengembalikan perubahan...

; *** Masalah secara umum
ErrorInternal2=Kesalahan internal: %1
ErrorFunctionFailedNoCode=%1 gagal
ErrorFunctionFailed=%1 gagal; kode %2
ErrorFunctionFailedWithMessage=%1 gagal; kode %2.%n%3
ErrorExecutingProgram=Pemasang tidak dapat mengeksekusi berkas:%n%1

; *** Masalah pada Registry
ErrorRegOpenKey=Kesalahan membuka kunci registry:%n%1\%2
ErrorRegCreateKey=Kesalahan membuat kunci registry:%n%1\%2
ErrorRegWriteKey=Kesalahan menulis ke kunci registry:%n%1\%2

; *** Masalah pada INI
ErrorIniEntry=Terjadi kesalahan saat membuat entri INI di berkas "%1".

; *** Masalah saat menyalin berkas
FileAbortRetryIgnoreSkipNotRecommended=&Lewati berkas ini (tidak disarankan)
FileAbortRetryIgnoreIgnoreNotRecommended=&Abaikan kesalahan dan lanjutkan (tidak disarankan)
SourceIsCorrupted=Berkas sumber rusak
SourceVerificationFailed=Verifikasi berkas sumber gagal: %1
VerificationSignatureDoesntExist=Berkas tanda tangan "%1" tidak ada
VerificationSignatureInvalid=Berkas tanda tangan "%1" tidak valid
VerificationKeyNotFound=Berkas tanda tangan "%1" menggunakan kunci yang tidak dikenal
VerificationFileNameIncorrect=Nama berkas tidak benar
VerificationFileTagIncorrect=Tag berkas tidak benar
VerificationFileSizeIncorrect=Ukuran berkas tidak benar
VerificationFileHashIncorrect=Hash berkas tidak benar
SourceDoesntExist=Berkas "%1" tidak ada
ExistingFileReadOnly2=Tidak dapat mengganti berkas yang ada karena ditandai sebagai hanya-baca.
ExistingFileReadOnlyRetry=&Hapus atribut hanya-baca dari berkas dan coba lagi
ExistingFileReadOnlyKeepExisting=&Simpan berkas yang ada
ErrorReadingExistingDest=Terjadi kesalahan saat mencoba membaca berkas yang ada:
FileExistsSelectAction=Pilih tindakan
FileExists2=Berkas sudah ada.
FileExistsOverwriteExisting=&Timpa berkas yang ada
FileExistsKeepExisting=&Simpan berkas yang ada
FileExistsOverwriteOrKeepAll=&Lakukan ini untuk konflik berikutnya
ExistingFileNewerSelectAction=Pilih tindakan
ExistingFileNewer2=Berkas yang ada lebih baru dari berkas yang akan diinstal oleh pemasang.
ExistingFileNewerOverwriteExisting=&Timpa berkas yang ada
ExistingFileNewerKeepExisting=&Simpan berkas yang ada (disarankan)
ExistingFileNewerOverwriteOrKeepAll=&Lakukan ini untuk konflik berikutnya
ErrorChangingAttr=Terjadi kesalahan saat mencoba mengubah atribut berkas yang ada:
ErrorCreatingTemp=Terjadi kesalahan saat mencoba membuat berkas di direktori tujuan:
ErrorReadingSource=Terjadi kesalahan saat mencoba membaca berkas sumber:
ErrorCopying=Terjadi kesalahan saat mencoba menyalin berkas:
ErrorDownloading=Terjadi kesalahan saat mencoba mengunduh berkas:
ErrorExtracting=Terjadi kesalahan saat mencoba mengekstrak berkas arsip:
ErrorReplacingExistingFile=Terjadi kesalahan saat mencoba mengganti berkas yang ada:
ErrorRestartReplace=Restart "Replace" gagal:
ErrorRenamingTemp=Terjadi kesalahan saat mencoba mengganti nama berkas di direktori tujuan:
ErrorRegisterServer=Tidak dapat mendaftarkan berkas DLL/OCX: %1
ErrorRegSvr32Failed=RegSvr32 gagal dengan kode keluar %1
ErrorRegisterTypeLib=Pemasang tidak dapat mendaftarkan pustaka tipe: %1

; *** Penandaan nama tampilan uninstall
; digunakan misalnya untuk 'Program Saya (32-bit)'
UninstallDisplayNameMark=%1 (%2)
; digunakan misalnya untuk 'Program Saya (32-bit, Semua pengguna)'
UninstallDisplayNameMarks=%1 (%2, %3)
UninstallDisplayNameMark32Bit=32-bit
UninstallDisplayNameMark64Bit=64-bit
UninstallDisplayNameMarkAllUsers=Semua pengguna
UninstallDisplayNameMarkCurrentUser=Pengguna saat ini

; *** Masalah pasca-instalasi
ErrorOpeningReadme=Terjadi kesalahan saat mencoba membuka berkas Readme.
ErrorRestartingComputer=Pemasang tidak dapat me-restart komputer. Harap lakukan secara manual.

; *** Pesan untuk Pelepas
UninstallNotFound=Berkas "%1" tidak ada. Tidak dapat melakukan uninstall.
UninstallOpenError=Tidak dapat membuka "%1". Tidak dapat melakukan uninstall.
UninstallUnsupportedVer=Berkas log uninstall "%1" dalam format yang tidak dikenal oleh versi uninstaller ini. Tidak dapat melakukan uninstall.
UninstallUnknownEntry=Entri tidak dikenal (%1) ditemukan di log uninstall
ConfirmUninstall=Apakah Anda yakin ingin menghapus %1 sepenuhnya, dan semua komponennya?
UninstallOnlyOnWin64=Instalasi ini hanya dapat di-uninstall pada Windows 64-bit.
OnlyAdminCanUninstall=Program ini hanya dapat di-uninstall oleh pengguna dengan hak istimewa administratif.
UninstallStatusLabel=Harap tunggu sementara %1 dihapus dari komputer Anda.
UninstalledAll=%1 berhasil dihapus dari komputer Anda.
UninstalledMost=%1 telah dihapus.%n%nBeberapa item tidak dapat dihapus. Dapat dihapus secara manual.
UninstalledAndNeedsRestart=Untuk menyelesaikan uninstall %1, komputer harus di-restart.%n%nRestart sekarang?
UninstallDataCorrupted=Berkas "%1" rusak. Tidak dapat melakukan uninstall

; *** Pesan fase uninstallation
ConfirmDeleteSharedFileTitle=Hapus Berkas Bersama?
ConfirmDeleteSharedFile2=Sistem menunjukkan bahwa berkas bersama berikut tidak lagi digunakan oleh program apa pun. Apakah Anda ingin pemasang menghapus berkas bersama ini?%n%nJika program masih menggunakan berkas ini dan dihapus, program tersebut mungkin tidak berfungsi dengan benar. Jika Anda tidak yakin, pilih Tidak. Membiarkan berkas di sistem Anda tidak akan menyebabkan kerusakan.
SharedFileNameLabel=Nama berkas:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status Uninstall
StatusUninstalling=Melakukan uninstall %1...

; *** Alasan pemblokiran shutdown
ShutdownBlockReasonInstallingApp=Menginstal %1.
ShutdownBlockReasonUninstallingApp=Melakukan uninstall %1.

; Pesan khusus berikut tidak digunakan oleh Pemasang itu sendiri, 
; namun bila Anda memakainya di dalam skrip Anda, maka terjemahkan.

[CustomMessages]

NameAndVersion=%1 versi %2
AdditionalIcons=Pintasan tambahan:
CreateDesktopIcon=Buat pintasan di Desktop
CreateQuickLaunchIcon=Buat pintasan Quick Launch
ProgramOnTheWeb=%1 di web
UninstallProgram=Uninstall %1
LaunchProgram=Jalankan %1
AssocFileExtension=Asosiasikan %1 dengan ekstensi berkas %2
AssocingFileExtension=Mengasosiasikan %1 dengan ekstensi berkas %2...
AutoStartProgramGroupDescription=Startup:
AutoStartProgram=Jalankan %1 secara otomatis
AddonHostProgramNotFound=Tidak dapat menemukan %1 di lokasi yang Anda pilih.%n%nLanjutkan tetap?