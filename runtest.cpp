#include <dirent.h>
#include <tchar.h>
#include <windows.h>
#include <stdio.h>

bool checkexe(char const * str){
	size_t slen=strlen(str);
	if (slen<4) return false;
	if (!strcmp(str+slen-4,".exe")) return true;
	return false;
}

void GetAndPrintAllFile(TCHAR const * path){
	TCHAR str[512]=_T(""),tmp[512]=_T("");
	_stprintf(tmp,_T("%s\\*"),path);
	{
		WIN32_FIND_DATA wfd={0};
		HANDLE hdFile=FindFirstFile(tmp,&wfd);
		if (hdFile==INVALID_HANDLE_VALUE && GetLastError()!=ERROR_FILE_NOT_FOUND)
			_tprintf(TEXT("FindFirstFile() Error!(%ld)\n"),GetLastError()),
		MessageBox(NULL,TEXT("FindFirstFile() Error!"),TEXT("Error!"),MB_ICONSTOP|MB_SETFOREGROUND),abort();
//		if (GetLastError()==ERROR_FILE_NOT_FOUND) goto closef;
		do {
			if (!(wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) continue;
//			printf("%s\n",wfd.cFileName);
			if (!_tcscmp(_T("."),wfd.cFileName)) continue;
			if (!_tcscmp(_T(".."),wfd.cFileName)) continue;
//			GetCurrentDirectory(1000,tmp);
			_stprintf(str,_T("%s\\%s"),path,wfd.cFileName);
			GetAndPrintAllFile(str);
		} while (FindNextFile(hdFile,&wfd));
		FindClose(hdFile);
	}
	SetCurrentDirectory(path);
	DIR * currentdir=opendir(".");
	dirent *nex=NULL;
	if (!currentdir) _tprintf(("opendir() Error!\n"));
	while ((nex=readdir(currentdir))!=NULL){/*
		if (!strcmp(".",nex->d_name)) continue;
		if (!strcmp("..",nex->d_name)) continue;*/
		if (!checkexe(nex->d_name)) continue;
#ifndef ONLINERUN
		_tprintf(_T("%s\\%s\n"),path,nex->d_name);
#else
		_stprintf(tmp,_T("%s\\%s"),path,nex->d_name);
		system(tmp);
#endif
	}
	closedir(currentdir);
	return ;
}

TCHAR a[122];

int _tmain(int argc, TCHAR const *argv[]){
//	printf("%d %d",checkexe("exe"),checkexe("aisndf.exe"));
//	GetAndPrintAllFile(_T("E:\\xdfj"));
	if (argc==1) 
	GetCurrentDirectory(122,a);
	_tprintf(_T("Current Dircetory:%s\n"),a);
//	if (argc!=1) _tfreopen(_T(argv[1]),_T("w"),stdout);
	GetAndPrintAllFile(argc==1?a:argv[1]);
}

