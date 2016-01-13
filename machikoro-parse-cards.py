#!/usr/bin/python

def escape(s):
	s = s.replace('"', '\\"')
	return s

def parse_normal(fields):
	(name,symbol,dice,cost,multisingle,color,description,edition)=fields
	dice_list = [d for d in dice.split(",")]
	s = ""
	s += '(make-normal-card :name "%s"\n' % escape(name)
	s += '                  :symbol %s\n' % (":"+symbol.lower())
	s += '                  :dice (list %s)\n' % " ".join(dice_list)
	s += '                  :cost %i\n' % int(cost)
	s += '                  :activation %s\n' % (":any" if multisingle=="m" else ":self")
	s += '                  :color %s\n' % (":"+color.lower())
	s += '                  :description "%s"\n' % escape(description)
	s += '                  :edition "%s")' % escape(edition)
	return s

def parse_large(fields):
	(name,symbol,cost,description,edition)=fields
	if name=="Rathaus": cost=0
	s = ""
	s += '(make-large-card :name "%s"\n' % escape(name)
	s += '                 :symbol %s\n' % (":"+symbol.lower())
	s += '                 :cost %i\n' % int(cost)
	s += '                 :description "%s"\n' % escape(description)
	s += '                 :edition "%s")' % escape(edition)
	return s

f = open("/home/toni/text/Machi Koro/cards.txt", "r")
cardmode="normal"
cards_normal = []
cards_large = []
for l in f:
	#print l
	l = l.strip()
	if l=="" or l[0]=='(':
		continue

	fields = l.split("\t")
	if fields[0] == "name":
		continue

	if cardmode=="normal":
		if len(fields)==5: cardmode="large"

	if cardmode=="normal":
		s = parse_normal(fields)
		cards_normal.append(s)

	elif cardmode=="large":
		s = parse_large(fields)
		cards_large.append(s)

out = open("/home/toni/lisp/machikoro-cards.lisp","w")

s = ''
s += '(let* ((placeholder (make-normal-card :dice \'(1) :cost 0 :activation :any))\n'
s += '       (c (make-array %i :element-type \'normal-card :initial-element placeholder)))\n' % len(cards_normal)
for i,c in enumerate(cards_normal):
	s += ('  (setf (aref c %i)\n' % i)+c+")\n"
s += '  (alexandria:define-constant +normal-cards+ c :test #\'equalp)\n'
s += '  (assert (null (find-if (lambda (x) (equal "" (normal-card-name x))) +normal-cards+))))\n'
out.write(s)

out.write("\n")

s = ''
s += '(let* ((placeholder (make-large-card :symbol :large :cost 0))\n'
s += '       (c (make-array %i :element-type \'large-card :initial-element placeholder)))\n' % len(cards_large)
for i,c in enumerate(cards_large):
	s += ('  (setf (aref c %i)\n' % i)+c+")\n"
s += '  (alexandria:define-constant +large-cards+ c :test #\'equalp)\n'
s += '  (assert (null (find-if (lambda (x) (equal "" (large-card-name x))) +large-cards+))))\n'
out.write(s)

out.close()
