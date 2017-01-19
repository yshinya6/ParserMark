#include<bits/stdc++.h>

#define REP(i,s,n) for(int i=s;i<n;i++)
#define rep(i,n) REP(i,0,n)

using namespace std;

int main(){
  freopen("xmark.cc","r",stdin);
  string s;
  while( getline(cin,s) ){
    if( s == "//*-*-*-*-*-" ) break;
    cout << s << endl;
  }
  return 0;
}
