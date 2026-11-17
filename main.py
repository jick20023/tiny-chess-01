import json,os,sys,urllib.request as U,urllib.error
E=os.environ.get("MAKEFUN_ENV_URL")or sys.exit(1)
V=dict(P=100,N=320,B=330,R=500,Q=900,K=2e4,p=-100,n=-320,b=-330,r=-500,q=-900,k=-2e4)
P=[[0]*8+[50]*8+[10,10,20,30,30,20,10,10,5,5,10,25,25,10,5,5,0,0,0,20,20,0,0,0,5,-5,-10,0,0,-10,-5,5,5,10,10,-20,-20,10,10,5]+[0]*8,
[-50,-40,-30,-30,-30,-30,-40,-50,-40,-20,0,0,0,0,-20,-40,-30,0,10,15,15,10,0,-30,-30,5,15,20,20,15,5,-30,-30,0,15,20,20,15,0,-30,-30,5,10,15,15,10,5,-30,-40,-20,0,5,5,0,-20,-40,-50,-40,-30,-30,-30,-30,-40,-50],
[-20,-10,-10,-10,-10,-10,-10,-20,-10,0,0,0,0,0,0,-10,-10,0,5,10,10,5,0,-10,-10,5,5,10,10,5,5,-10,-10,0,10,10,10,10,0,-10,-10,10,10,10,10,10,10,-10,-10,5,0,0,0,0,5,-10,-20,-10,-10,-10,-10,-10,-10,-20],
[0]*8+[5,10,10,10,10,10,10,5]+[-5,0,0,0,0,0,0,-5]*5+[0,0,0,5,5,0,0,0],
[-20,-10,-10,-5,-5,-10,-10,-20,-10,0,0,0,0,0,0,-10,-10,0,5,5,5,5,0,-10,-5,0,5,5,5,5,0,-5,0,0,5,5,5,5,0,-5,-10,5,5,5,5,5,0,-10,-10,0,5,0,0,0,0,-10,-20,-10,-10,-5,-5,-10,-10,-20],
[-30,-40,-40,-50,-50,-40,-40,-30]*4+[-20,-30,-30,-40,-40,-30,-30,-20,-10,-20,-20,-20,-20,-20,-20,-10,20,20,0,0,0,0,20,20,20,30,10,0,0,10,30,20]]
T="PNBRQK"
def post(p,b):
 r=U.Request(E+p,data=json.dumps(b).encode(),headers={"content-type":"application/json"})
 try:
  with U.urlopen(r)as x:return json.load(x)
 except:sys.exit(1)
def bd(f):
 B=[0]*64;s=f.split()
 for i,row in enumerate(s[0].split("/")):
  c=0
  for x in row:
   if x.isdigit():c+=int(x)
   else:B[i*8+c]=x;c+=1
 return B,s[1]=="w"
def ev(B,w):
 s=0
 for i,p in enumerate(B):
  if p:s+=V[p]+P[T.index(p.upper())][i if p.isupper()else i^56]*(1 if p.isupper()else -1)
 return s if w else -s
def sq(r,c):return r*8+c if 0<=r<8 and 0<=c<8 else -1
def gen(B,w):
 m=[]
 for i,p in enumerate(B):
  if not p or p.isupper()!=w:continue
  r,c=i>>3,i&7;t=p.upper()
  if t=="P":
   d=-1 if w else 1;n=sq(r+d,c)
   if n>=0 and not B[n]:
    if(w and r+d==0)or(not w and r+d==7):
     for x in"QRBN":m+=[(i,n,x if w else x.lower())]
    else:m+=[(i,n,0)]
    if(w and r==6)or(not w and r==1):
     n2=sq(r+2*d,c)
     if n2>=0 and not B[n2]:m+=[(i,n2,0)]
   for dc in(-1,1):
    n=sq(r+d,c+dc)
    if n>=0 and B[n]and B[n].isupper()!=w:
     if(w and r+d==0)or(not w and r+d==7):
      for x in"QRBN":m+=[(i,n,x if w else x.lower())]
     else:m+=[(i,n,0)]
  elif t=="N":
   for dr,dc in((-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)):
    n=sq(r+dr,c+dc)
    if n>=0 and(not B[n]or B[n].isupper()!=w):m+=[(i,n,0)]
  elif t in"BRQ":
   D=[(-1,-1),(-1,1),(1,-1),(1,1)]*(t in"BQ")+[(-1,0),(1,0),(0,-1),(0,1)]*(t in"RQ")
   for dr,dc in D:
    rr,cc=r+dr,c+dc
    while 1:
     n=sq(rr,cc)
     if n<0:break
     if B[n]:
      if B[n].isupper()!=w:m+=[(i,n,0)]
      break
     m+=[(i,n,0)];rr+=dr;cc+=dc
  else:
   for dr in(-1,0,1):
    for dc in(-1,0,1):
     if dr|dc:
      n=sq(r+dr,c+dc)
      if n>=0 and(not B[n]or B[n].isupper()!=w):m+=[(i,n,0)]
 return m
def ap(B,m):
 f,t,p=m;n=B[:];n[t]=p or n[f];n[f]=0;return n
def u(m):
 f,t,p=m
 return"abcdefgh"[f&7]+str(8-(f>>3))+"abcdefgh"[t&7]+str(8-(t>>3))+(p.lower()if p else"")
def ab(B,w,d,a,b,N):
 if N[0]<=0 or d==0:return ev(B,w)
 N[0]-=1
 ms=gen(B,w);ms.sort(key=lambda m:-abs(V.get(B[m[1]]or"",0)))
 best=-9e9 if w else 9e9
 for m in ms:
  sc=ab(ap(B,m),not w,d-1,a,b,N)
  if w:
   if sc>best:best=sc
   if best>a:a=best
   if a>=b:return best
  else:
   if sc<best:best=sc
   if best<b:b=best
   if a>=b:return best
 return best if ms else ev(B,w)
def choose(fen,legal):
 B,w=bd(fen);L=set(legal);c=[(m,u(m))for m in gen(B,w)if u(m)in L]
 if not c:return legal[0]
 N=[8000];bs=-9e9 if w else 9e9;best=legal[0]
 for m,x in c:
  sc=ab(ap(B,m),not w,2,-9e9,9e9,N)
  if(w and sc>bs)or(not w and sc<bs):bs=sc;best=x
 return best
while 1:
 g=post("/game",{})
 if g["status"]=="done":break
 while g["status"]=="your_move":
  g=post("/move",{"game":g["game"],"move":choose(g["fen"],g["legal_moves"])})
 print(f"g{g['game']}:{g['outcome']}",file=sys.stderr)
print(f"done {g.get('wins')}w{g.get('draws')}d{g.get('losses')}l",file=sys.stderr)
