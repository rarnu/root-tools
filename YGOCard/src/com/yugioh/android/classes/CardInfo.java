package com.yugioh.android.classes;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class CardInfo implements Serializable {

	private int _id;
	private int CardID;
	private String CardPhAl;
	private String CardCamp;
	private String JPCardName;
	private String SCCardName;
	private String TCCardName;
	private String ENCardName;
	private String ENCardName2;
	private String JPCardType;
	private String SCCardType;
	private String TCCardType;
	private String ENCardType;
	private String JPDCardType;
	private String SCDCardType;
	private String TCDCardType;
	private String ENDCardType;
	private String JPCardRace;
	private String SCCardRace;
	private String TCCardRace;
	private String ENCardRace;
	private String CardBagNum;
	private String JPCardAttribute;
	private String SCCardAttribute;
	private String TCCardAttribute;
	private String ENCardAttribute;
	private int CardStarNum;
	private String SCCardRare;
	private String TCCardRare;
	private String ENCardRare;
	private int CardAtk;
	private String CardAtk2;
	private int CardDef;
	private String CardDef2;
	private String JPCardDepict;
	private String SCCardDepict;
	private String TCCardDepict;
	private String ENCardDepict;
	private String SCCardBan;
	private String TCCardBan;
	private String ENCardBan;
	private int CardIsYKDT;
	private int CardIsTKEN;
	private int CardIsCZN;
	private String CardPass;
	private String CardAdjust;
	private int CardLover;
	private String CardUnion;
	private String CardOnceName;
	private String CardAbbrName;
	private String CardEfficeType;

	public int getCardID() {
		return CardID;
	}

	public void setCardID(int cardID) {
		CardID = cardID;
	}

	public String getCardPhAl() {
		return CardPhAl;
	}

	public void setCardPhAl(String cardPhAl) {
		CardPhAl = cardPhAl;
	}

	public String getCardCamp() {
		return CardCamp;
	}

	public void setCardCamp(String cardCamp) {
		CardCamp = cardCamp;
	}

	public String getJPCardName() {
		return JPCardName;
	}

	public void setJPCardName(String jPCardName) {
		JPCardName = jPCardName;
	}

	public String getSCCardName() {
		return SCCardName;
	}

	public void setSCCardName(String sCCardName) {
		SCCardName = sCCardName;
	}

	public String getTCCardName() {
		return TCCardName;
	}

	public void setTCCardName(String tCCardName) {
		TCCardName = tCCardName;
	}

	public String getENCardName() {
		return ENCardName;
	}

	public void setENCardName(String eNCardName) {
		ENCardName = eNCardName;
	}

	public String getENCardName2() {
		return ENCardName2;
	}

	public void setENCardName2(String eNCardName2) {
		ENCardName2 = eNCardName2;
	}

	public String getJPCardType() {
		return JPCardType;
	}

	public void setJPCardType(String jPCardType) {
		JPCardType = jPCardType;
	}

	public String getSCCardType() {
		return SCCardType;
	}

	public void setSCCardType(String sCCardType) {
		SCCardType = sCCardType;
	}

	public String getTCCardType() {
		return TCCardType;
	}

	public void setTCCardType(String tCCardType) {
		TCCardType = tCCardType;
	}

	public String getENCardType() {
		return ENCardType;
	}

	public void setENCardType(String eNCardType) {
		ENCardType = eNCardType;
	}

	public String getJPDCardType() {
		return JPDCardType;
	}

	public void setJPDCardType(String jPDCardType) {
		JPDCardType = jPDCardType;
	}

	public String getSCDCardType() {
		return SCDCardType;
	}

	public void setSCDCardType(String sCDCardType) {
		SCDCardType = sCDCardType;
	}

	public String getTCDCardType() {
		return TCDCardType;
	}

	public void setTCDCardType(String tCDCardType) {
		TCDCardType = tCDCardType;
	}

	public String getENDCardType() {
		return ENDCardType;
	}

	public void setENDCardType(String eNDCardType) {
		ENDCardType = eNDCardType;
	}

	public String getJPCardRace() {
		return JPCardRace;
	}

	public void setJPCardRace(String jPCardRace) {
		JPCardRace = jPCardRace;
	}

	public String getSCCardRace() {
		return SCCardRace;
	}

	public void setSCCardRace(String sCCardRace) {
		SCCardRace = sCCardRace;
	}

	public String getTCCardRace() {
		return TCCardRace;
	}

	public void setTCCardRace(String tCCardRace) {
		TCCardRace = tCCardRace;
	}

	public String getENCardRace() {
		return ENCardRace;
	}

	public void setENCardRace(String eNCardRace) {
		ENCardRace = eNCardRace;
	}

	public String getCardBagNum() {
		return CardBagNum;
	}

	public void setCardBagNum(String cardBagNum) {
		CardBagNum = cardBagNum;
	}

	public String getJPCardAttribute() {
		return JPCardAttribute;
	}

	public void setJPCardAttribute(String jPCardAttribute) {
		JPCardAttribute = jPCardAttribute;
	}

	public String getSCCardAttribute() {
		return SCCardAttribute;
	}

	public void setSCCardAttribute(String sCCardAttribute) {
		SCCardAttribute = sCCardAttribute;
	}

	public String getTCCardAttribute() {
		return TCCardAttribute;
	}

	public void setTCCardAttribute(String tCCardAttribute) {
		TCCardAttribute = tCCardAttribute;
	}

	public String getENCardAttribute() {
		return ENCardAttribute;
	}

	public void setENCardAttribute(String eNCardAttribute) {
		ENCardAttribute = eNCardAttribute;
	}

	public int getCardStarNum() {
		return CardStarNum;
	}

	public void setCardStarNum(int cardStarNum) {
		CardStarNum = cardStarNum;
	}

	public String getSCCardRare() {
		return SCCardRare;
	}

	public void setSCCardRare(String sCCardRare) {
		SCCardRare = sCCardRare;
	}

	public String getTCCardRare() {
		return TCCardRare;
	}

	public void setTCCardRare(String tCCardRare) {
		TCCardRare = tCCardRare;
	}

	public String getENCardRare() {
		return ENCardRare;
	}

	public void setENCardRare(String eNCardRare) {
		ENCardRare = eNCardRare;
	}

	public int getCardAtk() {
		return CardAtk;
	}

	public void setCardAtk(int cardAtk) {
		CardAtk = cardAtk;
	}

	public String getCardAtk2() {
		return CardAtk2;
	}

	public void setCardAtk2(String cardAtk2) {
		CardAtk2 = cardAtk2;
	}

	public int getCardDef() {
		return CardDef;
	}

	public void setCardDef(int cardDef) {
		CardDef = cardDef;
	}

	public String getCardDef2() {
		return CardDef2;
	}

	public void setCardDef2(String cardDef2) {
		CardDef2 = cardDef2;
	}

	public String getJPCardDepict() {
		return JPCardDepict;
	}

	public void setJPCardDepict(String jPCardDepict) {
		JPCardDepict = jPCardDepict;
	}

	public String getSCCardDepict() {
		return SCCardDepict;
	}

	public void setSCCardDepict(String sCCardDepict) {
		SCCardDepict = sCCardDepict;
	}

	public String getTCCardDepict() {
		return TCCardDepict;
	}

	public void setTCCardDepict(String tCCardDepict) {
		TCCardDepict = tCCardDepict;
	}

	public String getENCardDepict() {
		return ENCardDepict;
	}

	public void setENCardDepict(String eNCardDepict) {
		ENCardDepict = eNCardDepict;
	}

	public String getSCCardBan() {
		return SCCardBan;
	}

	public void setSCCardBan(String sCCardBan) {
		SCCardBan = sCCardBan;
	}

	public String getTCCardBan() {
		return TCCardBan;
	}

	public void setTCCardBan(String tCCardBan) {
		TCCardBan = tCCardBan;
	}

	public String getENCardBan() {
		return ENCardBan;
	}

	public void setENCardBan(String eNCardBan) {
		ENCardBan = eNCardBan;
	}

	public int getCardIsYKDT() {
		return CardIsYKDT;
	}

	public void setCardIsYKDT(int cardIsYKDT) {
		CardIsYKDT = cardIsYKDT;
	}

	public int getCardIsTKEN() {
		return CardIsTKEN;
	}

	public void setCardIsTKEN(int cardIsTKEN) {
		CardIsTKEN = cardIsTKEN;
	}

	public int getCardIsCZN() {
		return CardIsCZN;
	}

	public void setCardIsCZN(int cardIsCZN) {
		CardIsCZN = cardIsCZN;
	}

	public String getCardPass() {
		return CardPass;
	}

	public void setCardPass(String cardPass) {
		CardPass = cardPass;
	}

	public String getCardAdjust() {
		return CardAdjust;
	}

	public void setCardAdjust(String cardAdjust) {
		CardAdjust = cardAdjust;
	}

	public int getCardLover() {
		return CardLover;
	}

	public void setCardLover(int cardLover) {
		CardLover = cardLover;
	}

	public String getCardUnion() {
		return CardUnion;
	}

	public void setCardUnion(String cardUnion) {
		CardUnion = cardUnion;
	}

	public String getCardOnceName() {
		return CardOnceName;
	}

	public void setCardOnceName(String cardOnceName) {
		CardOnceName = cardOnceName;
	}

	public String getCardAbbrName() {
		return CardAbbrName;
	}

	public void setCardAbbrName(String cardAbbrName) {
		CardAbbrName = cardAbbrName;
	}

	public String getCardEfficeType() {
		return CardEfficeType;
	}

	public void setCardEfficeType(String cardEfficeType) {
		CardEfficeType = cardEfficeType;
	}

	public String toString() {

		String ret = "";
		try {
			Class<?> cls = this.getClass();
			Field[] fs = cls.getDeclaredFields();

			Method mGet = null;
			for (Field f : fs) {
				mGet = cls.getMethod("get" + f.getName(), new Class[] {});
				if (f.getType().getName().contains("String")) {
					ret = ret + f.getName() + ": "
							+ ((String) mGet.invoke(this, new Object[] {}));
				} else {
					ret = ret
							+ f.getName()
							+ ": "
							+ ((Integer) mGet.invoke(this, new Object[] {}))
									.toString();
				}
				ret = ret + "\n";

			}
		} catch (Exception e) {

		}

		return ret;
	}

	public void set_id(int _id) {
		this._id = _id;
	}

	public int get_id() {
		return _id;
	}

}
