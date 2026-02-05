import { NextRequest, NextResponse } from "next/server";
import { promises as fs } from "fs";
import path from "path";

const DATA_DIR = path.join(process.cwd(), "data");
const CONTACTS_FILE = path.join(DATA_DIR, "contacts.json");

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const { name, email, company, message, loc } = body;

    if (!name || !email || !message) {
      return NextResponse.json(
        { error: "Name, email, and message are required." },
        { status: 400 }
      );
    }

    const entry = {
      id: crypto.randomUUID(),
      name: String(name).slice(0, 200),
      email: String(email).slice(0, 200),
      company: company ? String(company).slice(0, 200) : "",
      message: String(message).slice(0, 5000),
      loc: loc ? String(loc).slice(0, 100) : "",
      timestamp: new Date().toISOString(),
    };

    await fs.mkdir(DATA_DIR, { recursive: true });

    let contacts: unknown[] = [];
    try {
      const data = await fs.readFile(CONTACTS_FILE, "utf-8");
      contacts = JSON.parse(data);
    } catch {
      // File doesn't exist yet
    }

    contacts.push(entry);
    await fs.writeFile(CONTACTS_FILE, JSON.stringify(contacts, null, 2));

    return NextResponse.json({ success: true, id: entry.id });
  } catch {
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 }
    );
  }
}
