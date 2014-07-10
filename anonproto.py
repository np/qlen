from random  import randint
from curve   import Curve
from point   import Point
from keypair import KeyPair
from bignum  import BigNum
#import hashlib

# TODO: move this to pyelliptic
from pyelliptic.openssl import OpenSSL
import ctypes
OpenSSL.BN_mod_inverse = OpenSSL._lib.BN_mod_inverse
OpenSSL.BN_mod_inverse.restype = ctypes.c_void_p
OpenSSL.BN_mod_inverse.argtypes = [ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p]

# TODO: move this to bignum
def BigNum_mod_inverse(a,n):
    ctx = OpenSSL.BN_CTX_new()
    a   = BigNum(decval=a)
    n   = BigNum(decval=n)
    try:
        return BigNum(os_bn=OpenSSL.BN_mod_inverse(0,a.bn,n.bn,ctx))
    finally:
        del a
        del n
        del ctx

curve = Curve('secp256k1')
G = curve.G

def check(P):
    a = P + P
    b = 2 * P
    c = P.dbl()
    print a
    print b
    print c
    assert (a == b)
    assert (c == b)
check(G)
check(7439794723947237947297 * G)
print(0 * G)
print(G)
print(1 * G)
exit(1)

def Enc(Pk, M):
    r = RandomBN()
    C0 = r * G
    C1 = r * Pk + M
    return (C0 , C1)

def Dec(sk, (C0 , C1)):
    return (C1 - sk * C0)

def inv(a):
    return BigNum_mod_inverse(a, curve.order)

def RandomBN():
    return randint(0, curve.order-1)

def RandomMAC():
    return randint(0, 2 ** 48-1)

def H(Y,Z,m):
    return Enc(Y, m * Z)

def F(y,z,C):
    return inv(z) * Dec(y, C)

def SetupServer(s):
    nonce = 'SECRET DEMTECH WHITE BOXES SURVEILLANCE SYSTEM'
    s.X = curve.hash_to_point(nonce)

    kY = KeyPair(curve)
    s.y = kY.private_key
    s.Y = kY.public_key

    s.z = RandomBN()
    s.Z = s.z * s.X

class ServerParams:
    def __init__(self):
        SetupServer(self)
        self.store = []

def Server():
    s = ServerParams()

    def ServeBox(C):
        print 'ServeBox'
        if C != None:
            n = F(s.y,s.z,C)
            s.store.append(n)
            return ServeBox
        else:
            return s.store

    return ((s.Y,s.Z), ServeBox)

def Box((Y,Z)):
    def cont():
        print 'Box/cont'
        return (H(Y,Z,RandomMAC()), cont)

    return cont()

def Go(S,(C,BK)):
    if C != None:
        SK = S(C)
        return Go(SK,BK())
    else:
        return S(C)

def Telecom(S,B):
    (M,SK) = S()
    return Go(SK,B(M))

class Check:
    def __init__(self):
        SetupServer(self)

    def checkFH_prop(self,m):
        if (F(self.y,self.z,H(self.Y,self.Z,m)) == m * self.X):
            print 'PASS'
        else:
            print 'FAIL'

    def someChecks(self):
        self.checkFH_prop(RandomMAC())
        self.checkFH_prop(RandomMAC())
        self.checkFH_prop(RandomMAC())
        self.checkFH_prop(RandomMAC())

Check().someChecks()
Telecom(Server,Box)
