package com.rarnu.command.emu.event;

public interface IEmuCallback {

	void openingEmu();
	void closingEmu();
	void receiveEmuMessage(String msg);
}
