#pragma once

#ifndef HEIGHT_BALANCED_TREE_H
#define HEIGHT_BALANCED_TREE_H

//	File:       HeightBalancedTree.h
//	Author:     Dimiter Georgiev (dimitertg@yahoo.com)
//	Date:       Fri Dec 2, 2005
//              Fri Dec 14, 2005, Chage: see FindComparable
//
//  Licence:    This file is in the public domain.
//              You may do whatever you want with it.
#include <minmax.h>
#include <string>
#include <stdexcept>
#include <locale>

template<typename DataType> struct NullDeleter
{
	inline static void DeleteData(const DataType &)
	{
		// do nothing.
	}
};

template<typename DataType, typename KeyType = DataType> struct DefaultComparator
{
	inline static int Compare(const KeyType & a, const DataType & b, const bool& SortLowToHigh)
	{
		if (a < b)
		{
			return SortLowToHigh ? -1 : 1;
		}
		if (b < a)
		{
			return SortLowToHigh ? 1 : -1;
		}
		return 0;
	}
};

template <typename T> T ToUpper(T c)
{
	static const std::locale CurrentLocale;
	static const std::ctype<T> & CurrentFacet =
		std::use_facet< std::ctype<T> > (CurrentLocale);
	return CurrentFacet.toupper(c);
}

template<typename StringType, typename KeyType = StringType> struct StringComparator
{
	static int Compare(const KeyType & a, const StringType & b, const bool& SortLowToHigh)
	{
		if (&a == &b)
		{
			return 0;
		}
		const size_t size = min(a.size(), b.size());
		for (size_t i = 0; i < size; ++i)
		{
			int aUpper = ToUpper(a[i]);
			int bUpper = ToUpper(b[i]);
			if (aUpper != bUpper)
			{
				return SortLowToHigh ? aUpper - bUpper : ((aUpper - bUpper) * -1);
			}
		}
		if (a.size() < b.size())
		{
			return SortLowToHigh ? -1 : 1;
		}
		if (a.size() > b.size())
		{
			return SortLowToHigh ? 1 : -1;
		}
		for (size_t i = 0; i < size; ++i)
		{
			int aChar = a[i];
			int bChar = b[i];
			if (aChar != bChar)
			{
				return SortLowToHigh ? aChar - bChar : (aChar - bChar) * -1;
			}
		}
		return 0;
	}
};

template <
	typename DataType,
	typename KeyType = DataType,
	typename Comparator = DefaultComparator<DataType, KeyType>,
	typename DataDeleter = NullDeleter<DataType>
>
class HeightBalancedTree
{
	class Node
	{
		Node * m_left;
		Node * m_right;
		int m_height;
		size_t m_size;
		DataType m_data;
	public:
		Node(const DataType & data):
			m_left(NULL),
			m_right(NULL),
			m_height(1),
			m_size(1),
			m_data(data)
		{
		}

		~Node()
		{
		}

		void FixStatistics()
		{
			this->m_height = max(GetHeight(m_left), GetHeight(m_right)) + 1;
			this->m_size = GetSize(m_left) + GetSize(m_right) + 1;
		}

		inline int GetHeight() const
		{
			return m_height;
		}

		inline size_t GetSize() const
		{
			return m_size;
		}

		inline Node * GetLeft() const
		{
			return m_left;
		}

		inline Node * GetRight() const
		{
			return m_right;
		}

		inline void SetLeft(Node * left)
		{
			this->m_left = left;
			FixStatistics();
		}

		inline void SetRight(Node * right)
		{
			this->m_right = right;
			FixStatistics();
		}

		inline DataType & GetData()
		{
			return m_data;
		}

		static inline int GetHeight(const Node * node)
		{
			if (node == NULL)
			{
				return 0;
			}
			return node->m_height;
		}

		static inline size_t GetSize(const Node * node)
		{
			if (node == NULL)
			{
				return 0;
			}
			return node->m_size;
		}

		static inline Node * GetLeft(const Node * node)
		{
			if (node == NULL)
			{
				return NULL;
			}
			return node->m_left;
		}

		static inline Node * GetRight(const Node * node)
		{
			if (node == NULL)
			{
				return NULL;
			}
			return node->m_right;
		}
	};
	// don't define these two:
	HeightBalancedTree(const HeightBalancedTree & );
	HeightBalancedTree& operator=(const HeightBalancedTree & );
public:

	HeightBalancedTree() //Default Sort is Low to High
	  : m_head(NULL),
		lowToHigh_(true)
	{
	}

	HeightBalancedTree(const bool& SortHighToLow)
		: m_head(NULL),
		lowToHigh_(!SortHighToLow)
	{
	}

	~HeightBalancedTree()
	{
		DelTree(m_head);
	}

	inline bool SortLowToHigh() const { return lowToHigh_; }
	inline void SetSortLowToHigh(const bool sortLowToHigh) { lowToHigh_ = sortLowToHigh; }

	inline size_t Size() const
	{
		return Node::GetSize(m_head);
	}

	inline int Height() const
	{
		return Node::GetHeight(m_head);
	}

	inline void InsertComparable(const DataType & obj)
	{
		m_head = InsertComparable(m_head, obj, lowToHigh_);
	}

	static inline Node * InsertComparable(
		Node * node,
		const DataType & obj,
		const bool& sortLowToHigh
	)
	{
		if (node == NULL)
		{
			Node * newNode = new Node(obj);
			return newNode;
		}
		int cmp = Comparator::Compare(obj, node->GetData(), sortLowToHigh);
		if (cmp <= 0)
		{
			node->SetLeft(InsertComparable(node->GetLeft(), obj, sortLowToHigh));
			return BringBalance(node);
		}
		else
		{
			node->SetRight(InsertComparable(node->GetRight(), obj, sortLowToHigh));
			return BringBalance(node);
		}
	}

	inline void InsertByIndex(size_t index, const DataType & obj)
	{
		m_head = InsertByIndex(m_head, index, obj);
	}

	static inline Node * InsertByIndex(
		Node * node,
		size_t index,
		const DataType & obj
	) {
		if (node == NULL)
		{
			Node * newNode = new Node(obj);
			return newNode;
		}
		size_t leftSize = Node::GetSize(node->GetLeft());
		if (index <= leftSize)
		{
			node->SetLeft(InsertByIndex(node->GetLeft(), index, obj));
			return BringBalance(node);
		}
		else
		{
			node->SetRight(InsertByIndex(node->GetRight(), index - leftSize - 1, obj));
			return BringBalance(node);
		}
	}

	// Bug fixed on December 14, 2005:
	// The returned index was wrong.
	// This function returns the index of the object
	// or Size() if the object is not present.
	inline bool FindComparable(const DataType & obj, size_t& returnPosition) const
	{
		Node * node = m_head;
		returnPosition = Node::GetSize(Node::GetLeft(node));
		while (node != NULL)
		{
			int cmp = Comparator::Compare(obj, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
				returnPosition = (returnPosition - Node::GetSize(Node::GetRight(node))) - 1;
			}
			else
			{
				node = node->GetRight();
				returnPosition = (returnPosition + Node::GetSize(Node::GetLeft(node))) + 1;
			}
		}
		return false;
	}

	inline bool FindComparable(const DataType & obj, DataType*& foundObject) const
	{
		Node * node = m_head;
		while (node != NULL)
		{
			int cmp = Comparator::Compare(obj, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				foundObject =  &(node->GetData());
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
			}
			else
			{
				node = node->GetRight();
			}
		}
		foundObject = NULL;
		return false;
	}

	inline bool FindComparable(const DataType & obj) const
	{
		Node * node = m_head;
		while (node != NULL)
		{
			int cmp = Comparator::Compare(obj, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
			}
			else
			{
				node = node->GetRight();
			}
		}
		return false;
	}

	inline bool FindByKey(const KeyType & findKey, DataType*& foundObject) const
	{
		Node * node = m_head;
		while (node != NULL)
		{
			int cmp = Comparator::Compare(findKey, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				foundObject = &(node->GetData());
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
			}
			else
			{
				node = node->GetRight();
			}
		}
		foundObject = NULL;
		return false;
	}

	inline bool FindByKey(const KeyType & findKey) const
	{
		Node * node = m_head;
		while (node != NULL)
		{
			int cmp = Comparator::Compare(findKey, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
			}
			else
			{
				node = node->GetRight();
			}
		}
		return false;
	}

	inline bool FindByKey(const KeyType & findKey, DataType*& foundObject, size_t& returnPosition) const
	{
		Node * node = m_head;
		returnPosition = Node::GetSize(Node::GetLeft(node));
		while (node != NULL)
		{
			int cmp = Comparator::Compare(findKey, node->GetData(), lowToHigh_);
			if (cmp == 0)
			{
				foundObject = &(node->GetData());
				return true;
			}
			if (cmp < 0)
			{
				node = node->GetLeft();
				returnPosition = (returnPosition - Node::GetSize(Node::GetRight(node))) - 1;
			}
			else
			{
				node = node->GetRight();
				returnPosition = (returnPosition + Node::GetSize(Node::GetLeft(node))) + 1;
			}
		}
		foundObject = NULL;
		return false;
	}

	inline DataType & FindByIndex(size_t index) const
	{
		Node * node = m_head;
		while (node != NULL)
		{
			size_t leftSize = Node::GetSize(node->GetLeft());
			if (index == leftSize)
			{
				return node->GetData();
			}
			if (index < leftSize)
			{
				node = node->GetLeft();
			}
			else
			{
				node = node->GetRight();
				index = index - leftSize - 1;
			}
		}
		throw std::out_of_range("HeightBalancedTree<T> index out of range");
	}

	inline void RemoveComparable(const DataType & obj)
	{
		m_head = RemoveComparable(m_head, obj, lowToHigh_);
	}

	static inline Node * RemoveComparable(
		Node * node,
		const DataType & obj,
		const bool& sortLowToHigh
	) {
		if (node == NULL)
		{
			return NULL;
		}
		int cmp = Comparator::Compare(obj, node->GetData(), sortLowToHigh);
		if (cmp == 0)
		{
			Node * left = node->GetLeft();
			Node * right = node->GetRight();
			if (right == NULL)
			{
				DelNode(node);
				return left;
			}
			if (left == NULL)
			{
				DelNode(node);
				return right;
			}
			Node * child = left;
			Node * parent = node;
			while (child->GetRight() != NULL)
			{
				parent = child;
				child = child->GetRight();
			}
			if (left == child)
			{
				node->SetLeft(left->GetLeft());
				node->SetRight(NULL);
				left->SetRight(right);
				left->SetLeft(node);
			}
			else
			{
				node->SetLeft(child->GetLeft());
				node->SetRight(NULL);
				parent->SetRight(node);
				child->SetLeft(left);
				child->SetRight(right);
			}
			node = child;
			--cmp;
		}
		if (cmp < 0)
		{
			node->SetLeft(RemoveComparable(node->GetLeft(), obj, sortLowToHigh));
			return BringBalance(node);
		}
		else
		{
			node->SetRight(RemoveComparable(node->GetRight(), obj, sortLowToHigh));
			return BringBalance(node);
		}
	}

	inline bool RemoveByIndex(size_t index)
	{
		bool result = index < Size();
		m_head = RemoveByIndex(m_head, index);
		return result;
	}

	static inline Node * RemoveByIndex(Node * node, size_t index)
	{
		if (node == NULL)
		{
			return NULL;
		}
		size_t leftSize = Node::GetSize(node->GetLeft());
		if (index == leftSize)
		{
			Node * left = node->GetLeft();
			Node * right = node->GetRight();
			if (right == NULL)
			{
				DelNode(node);
				return left;
			}
			if (left == NULL)
			{
				DelNode(node);
				return right;
			}
			Node * child = left;
			Node * parent = node;
			while (child->GetRight() != NULL)
			{
				parent = child;
				child = child->GetRight();
			}
			if (left == child)
			{
				node->SetLeft(left->GetLeft());
				node->SetRight(NULL);
				left->SetRight(right);
				left->SetLeft(node);
			}
			else
			{
				node->SetLeft(child->GetLeft());
				node->SetRight(NULL);
				parent->SetRight(node);
				child->SetLeft(left);
				child->SetRight(right);
			}
			node = child;
			--index;
		}
		if (index < leftSize)
		{
			node->SetLeft(RemoveByIndex(node->GetLeft(), index));
			return BringBalance(node);
		}
		else
		{
			node->SetRight(RemoveByIndex(node->GetRight(), index - leftSize - 1));
			return BringBalance(node);
		}
	}

	static inline Node * BringBalance(Node * node)
	{
		if (node == NULL)
		{
			return node;
		}
		int leftHeight = Node::GetHeight(node->GetLeft());
		int rightHeight = Node::GetHeight(node->GetRight());
		if (abs(leftHeight - rightHeight) < 2)
		{
			return node;
		}
		bool rotateRight = leftHeight > rightHeight;
		if (rotateRight)
		{
			Node * left = node->GetLeft();
			Node * leftChild = left->GetLeft();
			Node * rightChild = left->GetRight();
			int leftChildHeight = Node::GetHeight(leftChild);
			int rightChildHeight = Node::GetHeight(rightChild);
			if (leftChildHeight > rightChildHeight)
			{
				node->SetLeft(rightChild);
				left->SetRight(node);
				node = left;
			}
			else
			{
				node->SetLeft(rightChild->GetRight());
				left->SetRight(rightChild->GetLeft());
				rightChild->SetLeft(left);
				rightChild->SetRight(node);
				node = rightChild;
			}
		}
		else
		{
			Node * right = node->GetRight();
			Node * leftChild = right->GetLeft();
			Node * rightChild = right->GetRight();
			int leftChildHeight = Node::GetHeight(leftChild);
			int rightChildHeight = Node::GetHeight(rightChild);
			if (leftChildHeight < rightChildHeight)
			{
				node->SetRight(leftChild);
				right->SetLeft(node);
				node = right;
			}
			else
			{
				node->SetRight(leftChild->GetLeft());
				right->SetLeft(leftChild->GetRight());
				leftChild->SetRight(right);
				leftChild->SetLeft(node);
				node = leftChild;
			}
		}
		return node;
	}

	inline DataType & operator[](size_t index)
	{
		return FindByIndex(index);
	}

	template<typename T>
	class const_iteratorTmpl
	{
	private:

		size_t				currentIndex_;
		Node*				currentNode_;
		HeightBalancedTree* tree_;

	public:

		const_iteratorTmpl()
			: currentNode_(NULL),
			tree_(NULL),
			currentIndex_((size_t)-1)
		{}

		const_iteratorTmpl(const const_iteratorTmpl& copyIterator)
			: currentNode_(copyIterator.currentNode_),
			tree_(copyIterator.tree_),
			currentIndex_(copyIterator.currentIndex_)
		{}

		const_iteratorTmpl& operator=(const const_iteratorTmpl& assignIterator)
		{
			currentIndex_ = assignIterator.currentIndex_;
			currentNode_ = assignIterator.currentNode_;
			tree_ = assignIterator.tree_;
			return *this;
		}

		friend inline bool operator==(const const_iteratorTmpl& rhs, const const_iteratorTmpl& lhs)
		{
			if (rhs.tree_ == NULL || lhs.tree_ == NULL)
			{
				return rhs.currentNode_ == NULL && lhs.currentNode_ == NULL;
			}
			return rhs.tree_ == lhs.tree_ && rhs.currentNode_ == lhs.currentNode_;
		}

		friend inline bool operator!=(const const_iteratorTmpl& rhs, const const_iteratorTmpl& lhs)
		{
			return !(rhs == lhs);
		}

		inline const_iteratorTmpl& operator++()
		{
			++currentIndex_;
			FindNextNode();
			return *this;
		}

		inline const_iteratorTmpl operator++(int)
		{
			const_iteratorTmpl prevIterator = *this;
			++currentIndex_;
			FindNextNode();
			return prevIterator;
		}

		inline T& operator*()
		{
			return currentNode_->GetData();
		}

		inline T* operator->()
		{
			return &(currentNode_->GetData());
		}

		inline size_t index() const
		{
			return currentIndex_;
		}

		inline bool atend() const
		{
			return tree_ == NULL || currentNode_ == NULL;
		}

		inline T& get()
		{
			return currentNode_->GetData();
		}

		inline bool reset()
		{
			if (tree_ != NULL)
			{
				currentIndex_ = 0;
				FindNextNode();
				return true;
			}
			return false;
		}

	protected:

		friend class HeightBalancedTree;

		const_iteratorTmpl(const HeightBalancedTree* tree)
			: currentNode_(NULL),
			tree_(const_cast<HeightBalancedTree*>(tree)),
			currentIndex_(0)
		{
			FindNextNode();
		}

		inline void FindNextNode()
		{
			currentNode_ = tree_->m_head;
			size_t index = currentIndex_;
			size_t leftSize;

			while (currentNode_ != NULL)
			{
				leftSize = Node::GetSize(currentNode_->GetLeft());
				if (index == leftSize)
				{
					return;
				}
				if (index < leftSize)
				{
					currentNode_ = currentNode_->GetLeft();
				}
				else
				{
					currentNode_ = currentNode_->GetRight();
					index = index - leftSize - 1;
				}
			}
		}
	};

	typedef const_iteratorTmpl<DataType> const_iterator;

	inline const_iterator begin() const
	{
		return const_iterator(this);
	}

	inline const_iterator end() const
	{
		return const_iterator();
	}

private:

	friend class const_iterator;

	Node * m_head;

	static inline void DelTree(Node * node)
	{
		if (node == NULL)
		{
			return;
		}
		Node * left = node->GetLeft();
		Node * right = node->GetRight();
		DelNode(node);
		DelTree(left);
		DelTree(right);
	}

	static inline void DelNode(Node * node)
	{
		DataDeleter::DeleteData(node->GetData());
		delete node;
	}

	bool lowToHigh_;

};

#endif
