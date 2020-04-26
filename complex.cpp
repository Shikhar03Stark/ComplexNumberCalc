#include <iostream>
#include <cmath>
#include <string>
#include <cstdlib>
#include <map>
#include <stack>
#include <vector>
#include <algorithm>
#include <queue>
using namespace std;


template <typename T, typename U = double>class Complex{
	protected:
	T real, imaginary;
	public:
	//Constructors
	Complex(){
		real = 0;
		imaginary = 0;
	}
	Complex(T r, T i){
		real = r;
		imaginary = i;
	}
	Complex(Complex<T,U> &z){
		real = z.getPart(0);
		imaginary = z.getPart(1);
	}

	//getPart Function
	T getPart(bool part){
		if(part){
			return imaginary;
		}
		return real;
	}
	void setPart(bool part, T val){
		if(part){
			imaginary = val;
		}
		else{
			real = val;
		}
	}
	Complex<T,U> conjugate(){
		imaginary = - imaginary;
		return *(this);
	}
	//show Function
	string show(){
		string s;
		if(imaginary == T()){
			s = to_string(real) + "\n";
			return s;
		}
		if (real == T()){
			s = to_string(imaginary) + "i\n";
			return s;
		}
		if(imaginary < T()){
			s = to_string(real) + to_string(imaginary)+"i\n";
			return s;
		}
		else{
			s = to_string(real) + "+" + to_string(imaginary)+"i\n";
			return s;
		}
	}

	Complex& operator*(Complex z){
		Complex *w = new Complex(real*(z.getPart(0)) - imaginary*(z.getPart(1)), real*(z.getPart(1)) + imaginary*(z.getPart(0)));
		return (*w);
	}

	Complex& operator*(U scalar){
		Complex *w = new Complex(real*scalar, imaginary*scalar);
		return (*w);
	}

	Complex& operator~(){
		imaginary = -imaginary;
		return (*this);
	}

	Complex& operator+(Complex w){
		Complex* z = new Complex(real + w.getPart(0), imaginary + w.getPart(1));
		return (*z);
	}

	Complex& operator-(){
		Complex* z = new Complex(-real, -imaginary);
		return (*z);
	}

	Complex& rasie(U exp){
		Complex *z = new Complex(pow(magnitude(*this),exp)*cos(exp*arg(*this)), pow(magnitude(*this),exp)*sin(exp*arg(*this)));
		return (*z);
	}
};

template <typename T, typename U = double>
U magnitude(Complex<T> z){
	return (sqrt(z.getPart(0)*z.getPart(0) + z.getPart(1)*z.getPart(1)));
}

template <typename T, typename U = double>
Complex<T>& operator*(U scalar, Complex<T> z){
	Complex<T> *w = new Complex<T>(z.getPart(0)*scalar, z.getPart(1)*scalar);
	return (*w);
}

template <typename T, typename U = double>
U arg(Complex<T,U> z){
	T img = z.getPart(1);
	T r = z.getPart(0);
	if(img >= 0){
		if(r >= 0){
			return (U(atan(img/r)));
		}
		return (U(3.14159 - atan(-img/r)));
	}
	else{
		if(r >= 0){
			return (U(-atan(-img/r)));
		}
		return (U(atan(img/r) - 3.14159));
	}
}

bool OpPre(char l, char r){
	if(l == '(' || l == '{' || l == '['){
		return true;
	}
	if(l == '~'){
		if(r == '~'){
			return false;
		}
		return true;
	}
	if(l == '^'){
		if(r == '~' || r == '^'){
			return false;
		}
		return true;
	}
	if(l == '*' || l == '/'){
		if(r == '+' || r == '-'){
			return true;
		}
		return false;
	}
	if(l ==  '+' || l == '-'){
		return false;
	}
	return false;
}

template <typename T, typename U = double>
void evalInfix(string exp, map<string,Complex<T>*> vars, Complex<T>* res){
	//parse exp to compute result...
	//using Shunting Yard Algo by E. Dijkstra
	stack <char> ops;
	queue <string> output;
	string token = "";
	vector<char> spl = {'(','{','[',']','}',')','~','^','*','/','+','-'};
	sort(spl.begin(), spl.end());
	string str = "";
	for(char ch: exp){
		if(!binary_search(spl.begin(),spl.end(),ch)){
			token.push_back(ch);
			continue;
		}
		if(token.size() == 0){
			ops.push(ch);
			continue;
		}
		output.push(token);
		if(ops.empty()){
			ops.push(ch);
		}
		else{
			//string str=""; str.push_back(ch);
			while(ops.size() != 0 && !OpPre(ch,ops.top())){
				if(ops.top() == ')' || ops.top() == '}' || ops.top() == ']' || ops.top() == '(' || ops.top() == '{' || ops.top() == '['){
					ops.pop();
				}
				else{
					str.push_back(ops.top());
					output.push(str);
					str = "";
					ops.pop();
				}
			}

			ops.push(ch);
		}
		//cout << output.back() << endl;
		token = "";
	}
	if(!ops.empty()){
		str.push_back(ops.top());
		if(!(str == ")" || str == "}" || str == "]"))
			output.push(str);
		str = "";
		ops.pop();
	}
	//cout << output.back() << endl;
	stack <Complex<T>*> data;
	while(!output.empty()){
		token  = output.front();
		output.pop();
		if(token == "~" || token == "^" || token == "*" || token == "/" || token == "+" || token == "-"){
			if(token != "~"){
				Complex<T>* right = data.top(); data.pop();
				Complex<T>* left = data.top(); data.pop();
				if(token == "^"){
					Complex<T> res = (*left).rasie(right->getPart(0));
					data.push(&res);
				}
				if(token == "*"){
					Complex<T> res = (*left)*(*right);
					data.push(&res);
				}
				if(token == "/"){
					Complex<T> res = (*left)*(right->rasie(-1));
					data.push(&res);
				}
				if(token == "+"){
					Complex<T> res = (*left)+(*right);
					data.push(&res);
				}
				if(token == "-"){
					Complex<T> res = (*left)+(-(*right));
					data.push(&res);
				}
			}
			else{
				Complex<T>* right = data.top(); data.pop();
				//do error checking::::
				Complex<T> res = ~(*right);
				data.push(&res);
			}
		}
		else{
			bool isVar = false;
			for(auto p: vars){
				if(p.first == token){
					isVar = true;
					data.push(p.second);
					break;
				}
			}
			if(!isVar){
				bool isDecimal = false;
				int index = 0;
				for(;index < (token.size());index++){
					if(token[index] == '.'){
						isDecimal = true;
						break;
					}
				}
				if(!isDecimal){
					Complex<T>* temp = new Complex<double>(double(atoi(token.c_str())),0);
					data.push(temp);
				}
				else{
					string ldec = token.substr(0,index);
					string rdec = token.substr(index+1);
					double r = atoi(ldec.c_str()) + (atoi(rdec.c_str())/pow(10,token.size()-index-1));
					Complex<T>* temp = new Complex<double>(r,0);
					data.push(temp);
				}
			}
		}
	}
	Complex<T>* temp = data.top();
	res->setPart(0, temp->getPart(0));
	res->setPart(1, temp->getPart(1));
	data.pop();
}

template <typename T, typename U = double>
void evalExp(string exp, map<string,Complex<T>*> vars){
	//checking for balanced Brackets
	stack<char> bracket;
	for(char ch: exp){
		if(ch == ')' || ch == '}' || ch == ']'){
			if(bracket.empty()){
			cout << "The Expression is Not Valid.\n";
			cout << "Brackets are not Balanced...\n";
			return;	
			}
			if(ch == ')' && bracket.top() == '('){
				bracket.pop();
			}
			else if(ch == '}' && bracket.top() == '{'){
				bracket.pop();
			}
			else if(ch == ']' && bracket.top() == '['){
				bracket.pop();
			}
			else{
				cout << "The Expression is Not Valid.\n";
				cout << "Brackets are not Balanced...\n";
				return;
			}
		}
		if(ch == '(' || ch == '{' || ch == '['){
			bracket.push(ch);
		}
	}
	if(!bracket.empty()){
			cout << "The Expression is Not Valid.\n";
			cout << "Brackets are not Balanced...\n";
			return;
	}
	//Brackets are Balanced
	
	//check for valid variables
	vector<char> spl = {'(','{','[',')','}',']','+','-','*','/','^','~','0','1','2','3','4','5','6','7','8','9','.'};
	sort(spl.begin(),spl.end());
	string v="";
	for(char ch: exp){
		if(binary_search(spl.begin(),spl.end(),ch)){
			if(v.size() == 0){
				continue;
			}
			bool isFound = false;
			for(auto p: vars){
				if(p.first == v){
					v = "";
					isFound = true;
					break;
				}
			}
			if(!isFound){
				cout << "The Expression is not Valid.\n";
				cout << "Variable '" << v << "' is not defined...\n";
				return;
			}
		}
		else{
			v.push_back(ch);
		}
	}
	//exp string is valid
	Complex<double>* result = new Complex<double>();
	evalInfix(exp,vars, result);
	cout << ">> " << result->show() << endl;
	cout << ">> Magnitude: " << magnitude(*result) << endl;
	cout << ">> Arg: " << arg(*result) << endl;
}

int main(){
	system("cls");
	map<string, Complex<double>* > variables;
	while(true){
		cout << ":::::::The Complex Number Calculator::::::\n";
		cout << "Your Declared Complex Numbers:\n";
		for(auto p: variables){
			cout << p.first << " : " << (p.second)->show() << endl;
		}
		cout << "------------------" << endl;
		cout << "1. To Create/Edit Complex Variable \ttype 'create_edit'\n";
		cout << "2. To Remove a declared Complex Variable \ttype 'remove'\n";
		cout << "3. To see How to Use Operators and write Expression \ttype 'help'\n";
		cout << "4. To Enter an Expression \ttype 'input'\n";
		cout << "5. To Close the Calculator \ttype 'exit'\n";
		string str;
		cout << "$_> ";
		cin >> str;
		system("cls");
		if(str.compare("create_edit") == 0){
			cout << ":::::Creating/Editing Complex Variable:::::\n";
			cout << "Your Declared Complex Numbers:\n";
			for(auto p: variables){
				cout << p.first << " : " << (p.second)->show() << endl;
			}
			string name;
			double r, img;
			cout << "Enter Variable Name: ";
			cin >> name;
			cout << "Enter Real Part: ";
			cin >> r;
			cout << "Enter Imaginary Part: ";
			cin >> img;
			variables[name] = new Complex<double>(r,img);
			cout << "Variable created successfully...\n";
			system("pause");
			system("cls");
		}
		else if(str.compare("remove") == 0){
			cout << ":::::Remove a Complex Variable:::::\n";
			cout << "Your Declared Complex Numbers:\n";
			for(auto p: variables){
				cout << p.first << " : " << (p.second)->show() << endl;
			}
			char ch;
			string name;
			cout << "Enter the name of Complex Variable to delete: ";
			cin >> name;
			bool isFound = false;
			for(auto p: variables){
				if(p.first == name){
					isFound = true;
					break;
				}
			}
			if(isFound){
				cout << "Are you Sure you want to delete '" << name << "' ?[y/n] ";
				cin >> ch;
				if(ch == 'y'){
					variables.erase(name);
					cout << "Variable Removed...\n";
					system("pause");
				}
				else{
					cout << "Variable not Removed...\n";
					system("pause");
				}
				system("cls");
			}
			else{
				cout << "The '"<< name <<"' is not found in Declared list...\n";
				system("pause");
				system("cls");
			}
		}
		else if(str.compare("help") == 0){
			cout << "::::::HELP::::::\n";
			cout << "How to use this Calculator\n";
			cout << "\t1. This Calculator is based upon Console I/O with User.\n";
			cout << "\t2. You Can create or edit your 'Valid' Complex Variables that can Used in Expressions\n";
			cout << "\t3. Enter the Menu Commands after $_>\n";
			cout << "\t4. Enter the Expression to be evaluated in 'input' menu after >>\n";
			cout << "--------------------\n";
			cout << "Restriction on Variable Names:\n";
			cout << "\tA Complex Variable can't conatain numbers [0-9], decimal (.), operators {'+','-','*','/','^','~'} or any Brackets\n";
			cout << "\tIt may contain underscores and strings of English Lexicon.\n";
			cout << "\t'abc', 'x', 'x_y' <- are VALID Variable names\n";
			cout << "\t'z1', 'a.b', 'c[1]' <- are INVALID Variable Names\n";
			cout << "-------------------\n";
			cout << "This Calculator Uses the following Operators\n";
			cout << "+ - * / ^ ~\n";
			cout << "The '+' Operator:\n";
			cout << "\t+ is a binary operator and can be used between 'Complex Variables'\n\t(Complex_var)+(Complex_var)\n";
			cout << "The '-' Operator:\n";
			cout << "\t- is a binary Operator and can be used between 'Complex Variables'\n\t(Complex_var)-(Complex_var)\n";
			cout << "The '*' Operator:\n";
			cout << "\t* is a binary Operator and be used between 'Complex Variables' and 'Scalars' in any combination\n\t(Complex_var)*(Complex_var)\n\t(Scalar)*(Complex_var)\n\t(Complex_var)*(Scalar)\n";
			cout << "The '/' Operator:\n";
			cout << "\t/ is a binary Operator and be used between 'Complex Variables' and 'Scalars' in any combination\n\t(Complex_var)/(Complex_var)\n\t(Scalar)/(Complex_var)\n\t(Complex_var)/(Scalar)\n";
			cout << "The '^' Operator:\n";
			cout << "\t^ is a binary Operator and can be used strictly between 'Complex Variables' and 'Scalar'\n\t(Complex_var)^(Scalar)\n";
			cout << "The '~' Operator:\n";
			cout << "\t~ is a uniary Operator and can be used in prefix of (Complex_variable)\n\t~(Complex_Variable)\n";
			cout << "Precedence of Operator is as follows:\n";
			cout << "\tBRACKETS\n\t'~'\n\t'^'\n\t'*' '/'\n\t'+' '-'\n";
			cout << "Operators with Same Precedence Level will be evaluate from Left to Right\n";
			cout << "-------------------\n";
			cout << "Use the following syntax to Enter Expression\n";
			cout << "\t1. There should be (), {} or [] over whole expression.\n";
			cout << "\t2. No BLANK SPACE between Operators, Scalars and Complex Variables\n";
			cout << "\t3. Use brackets. (), {} and [] brackets are Allowed. Brackets should be Properly Balanced.\n";
			cout << "\t(z1+z2+(~(z3/z4))) is a Valid Expression\n";
			cout << "\ta+(b*2) is Invalid Expression, Use close brackets over whole expression\n";
			cout << "\t(a + ~(b/2)) is a Valid Expression\n";
			system("pause");
			system("cls");
		}
		else if(str.compare("input") == 0){
			cout << ":::::Enter Expression::::::\n";
			cout << "Your declared Complex Numbers:\n";
			for(auto p: variables){
				cout << p.first << " : " << p.second->show() << endl;
			}
			cout << "--------------------" << endl;
			cout << ">> ";
			string exp;
			cin >> exp;
			evalExp(exp,variables);
			system("pause");
			system("cls");
		}
		else if(str.compare("exit") == 0){
			system("pause");
			break;
		}
		else{
			cout << "Please Enter a valid KeyWord...\n";
			system("pause");
			system("cls");
		}
	}
	return 0;
}
