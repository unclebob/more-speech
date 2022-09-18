package ecdhJava;

import org.bouncycastle.asn1.sec.SECNamedCurves;
import org.bouncycastle.asn1.x9.X9ECParameters;
import org.bouncycastle.crypto.agreement.ECDHBasicAgreement;
import org.bouncycastle.crypto.params.ECDomainParameters;
import org.bouncycastle.crypto.params.ECPrivateKeyParameters;
import org.bouncycastle.crypto.params.ECPublicKeyParameters;
import org.bouncycastle.math.ec.ECPoint;
import org.bouncycastle.util.Arrays;
import org.bouncycastle.util.encoders.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.math.BigInteger;
import java.util.Random;

public class SECP256K1 {
  private static final X9ECParameters params;
  private static final ECDomainParameters curve;

  static {
    params= SECNamedCurves.getByName("secp256k1");
    curve= new ECDomainParameters(params.getCurve(), params.getG(), params.getN(), params.getH());
  }

  public static byte[] bytesFromBigInteger(BigInteger n) {
      byte[] b = n.toByteArray();

      if(b.length == 32) {
          return b;
      }
      else if(b.length > 32) {
          return java.util.Arrays.copyOfRange(b, b.length - 32, b.length);
      }
      else {
          byte[] buf = new byte[32];
          System.arraycopy(b, 0, buf, buf.length - b.length, b.length);
          return buf;
      }
  }

  public static BigInteger calculateKeyAgreement(BigInteger privKey, BigInteger theirPubKey) {
    ECPrivateKeyParameters privKeyP = new ECPrivateKeyParameters(privKey, curve);
    byte[] compressed = new byte[]{2};
    byte[] val = Arrays.concatenate(compressed, bytesFromBigInteger(theirPubKey));
    ECPoint ecPoint = curve.getCurve().decodePoint(val);
    ECPublicKeyParameters pubKeyP = new ECPublicKeyParameters(ecPoint, curve);

    ECDHBasicAgreement agreement = new ECDHBasicAgreement();
    agreement.init(privKeyP);
    return agreement.calculateAgreement(pubKeyP);
  }

  public static String encrypt(BigInteger key, String msg) throws Exception {
    Random r = new Random();
    byte[] iv = new byte[16];
    r.nextBytes(iv);
    Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(bytesFromBigInteger(key), "AES"), new IvParameterSpec(iv));
    String ivBase64 = Base64.toBase64String(iv);
    byte[] encryptedMsg = cipher.doFinal(msg.getBytes());
    String encryptedMsgBase64 = Base64.toBase64String(encryptedMsg);
    return String.format("%s?iv=%s", encryptedMsgBase64, ivBase64);
  }

  public static String decrypt(BigInteger key, String encryptedMsg) throws Exception {
    int pos = encryptedMsg.indexOf("?iv=");
    String msgPart = encryptedMsg.substring(0,pos);
    String ivPart = encryptedMsg.substring(pos+4);
    byte[] decodedMsg = Base64.decode(msgPart);
    byte[] iv = Base64.decode(ivPart);
    Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(bytesFromBigInteger(key), "AES"), new IvParameterSpec(iv));
    return new String(cipher.doFinal(decodedMsg));
  }
}